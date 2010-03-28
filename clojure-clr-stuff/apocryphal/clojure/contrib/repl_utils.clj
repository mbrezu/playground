
; Original copyright notice from clojure.contrib.repl_utils.clj:

;   Copyright (c) Chris Houser, Dec 2008. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Common Public License 1.0 (http://opensource.org/licenses/cpl.php)
;   which can be found in the file CPL.TXT at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

; Utilities meant to be used interactively at the REPL

(ns apocryphal.clojure.contrib.repl-utils
  (:use apocryphal.clojure.contrib.clr-macros)
  (:use apocryphal.clojure.contrib.str-utils)
  (:use apocryphal.clojure.contrib.seq-utils)
  (:import System.IO.StreamReader)
  (:import System.IO.TextReader)
  (:import clojure.lang.PushbackTextReader)
  (:import clojure.lang.Compiler)
  (:import System.Text.RegularExpressions.Regex))

(defstruct <reflection-row> :name :row :sort-val)

(defn- parameter-to-string [parameter]
  (str (if (.IsOut parameter)
         "out ")
       (if (.IsRetval parameter)
         "retval "
         "")
       (.. parameter ParameterType Name)
       " "
       (.Name parameter)))

(defn- method-to-row [method]
  (struct <reflection-row>
          (.Name method)
          (str (if (.IsStatic method)
                 "static ")
               (.Name method)
               (if (not (.IsConstructor method))
                 (if (not (= System.Void (.ReturnType method)))
                   (str " : " (.. method ReturnType Name))))
               " ("
               (str-join ", " (map parameter-to-string (.GetParameters method)))
               ")")
          [(not (.IsStatic method)) (not (.IsConstructor method)) false]))

(defn- property-to-row [property]
  (struct <reflection-row>
          (.Name property)
          (str (.Name property)
               " : "
               (.. property PropertyType Name)
               (let [index-parameters (.GetIndexParameters property)]
                 (if (> (count (seq index-parameters)) 0)
                   (str "["
                        (str-join ", " (map parameter-to-string index-parameters))
                        "]")))
               " { "
               (if (.CanRead property)
                 "get; ")
               (if (.CanWrite property)
                 "set; ")
               "}")
          [true true true]))

(defn show
  "This is a quick adaptation of show in
  clojure.contrib.repl-utils. Will need to convert the dependencies of
  repl-utils in clojure.contrib to get a proper conversion (which
  would be easily updatable when the original clojure.contrib
  changes).

  With one arg prints all static and instance members of x or (class
  x).  Each member is listed with a number which can be given as
  'selector' to return the member object -- the REPL will print more
  details for that member.

  The selector also may be a string or regex, in which case only
  members whose names match 'selector' as a case-insensitive regex
  will be printed.

  Finally, the selector also may be a predicate, in which case only
  members for which the predicate returns true will be printed.  The
  predicate will be passed a single argument, a map that includes the
  :text that will be printed and the :member object itself, as well as
  all the properies of the member object as translated by 'bean'.

  Examples: (show Integer) (show []) (show String 23) (show String \"case\")"
  ([x] (show x (constantly true)))
  ([x selector]
     (let [type-of-object (if (class? x) x (type x))
           constructors (.GetConstructors type-of-object)
           methods (.GetMethods type-of-object)
           properties (.GetProperties type-of-object)
           members (indexed (sort-by :sort-val
                                     (concat (map method-to-row constructors)
                                             (map method-to-row methods)
                                             (map property-to-row properties))))]
       (if (number? selector)
         (-> members (nth selector) second :row)
         (let [pred (if (ifn? selector)
                      selector
                      #(re-find (re-pattern (str "(?i)" selector)) (:name %)))]
           (println "=== " type-of-object " ===")
           (doseq [[i m] members]
             (when (pred m)
               (printf "[%3d] %s\n" i (m :row)))))))))

(defmacro show-val
  "Shows the information about the value, but also the value."
  [expr]
  `(let [expr# ~expr]
     (do
       (println expr#)
       (show expr#))))

(defn get-source
  "Returns a string of the source code for the given symbol, if it can
  find it.  This requires that the symbol resolve to a Var defined in
  a namespace for which the .clj is in the classpath.  Returns nil if
  it can't find the source.  For most REPL usage, 'source' is more
  convenient.

  Example: (get-source 'filter)"
  [x]
  (when-let [v (resolve x)]
    (when-let [filepath (:file (meta v))]
      (when-let [line (:line (meta v))]
        (with-open [rdr (StreamReader. filepath)]
          (dotimes [_ (dec line)] (.ReadLine rdr))
          (let [text (StringBuilder.)
                pbr (proxy [PushbackTextReader] [rdr]
                      (Read [] (let [i (proxy-super Read)]
                                 (.Append text (char i))
                                 i)))]
            (read (PushbackTextReader. pbr))
            (.ToString text)))))))

(defmacro source
  "Prints the source code for the given symbol, if it can find it.
  This requires that the symbol resolve to a Var defined in a
  namespace for which the .clj is in the classpath.

  Example: (source filter)"
  [n]
  `(println (or (get-source '~n) (str "Source not found"))))

;; `expression-info` was ported from the original repl-utils sources
;; by David Miller.
(defn expression-info [expr]
 (let [expr-ast (clojure.lang.Compiler/GenerateAST expr false)]
   (when (.HasClrType expr-ast)
     { :class (.ClrType expr-ast)
       :primitive? (.IsPrimitive (.ClrType expr-ast))})))

(defn stack-trace []
  (when *e
    (let [lines (re-split (Regex. "\n") (.get_StackTrace *e))]
      (doseq [line lines]
        (println line)))))

;; .NET specific utilities

(defn ls-assemblies
  "Display the currently loaded assemblies."
  []
  (doseq [k (map #(.. % GetName Name)
                 (seq (.GetAssemblies AppDomain/CurrentDomain)))]
    (println k)))

(defn- get-assembly-directories
  "Provides a list of directories to search assemblies for."
  []
  (cons *compile-path*
        (re-split #";" (.. (Environment/GetEnvironmentVariables)
                           (get_Item "clojure.load.path")))))

(defn load-assembly
  "Loads an assembly given a name. Looks in directories listed by
  environment variable clojure.load.path and in *compile-path*.

  If no file is found and the caller didn't ask specifically for a dll
  tries to generate the assembly by compiling."

  [assembly-name]
  (let [dirs (get-assembly-directories)
        provided-dll (.EndsWith (.ToUpper assembly-name) ".DLL")
        assembly-file-name (if provided-dll
                             assembly-name
                             (str assembly-name ".dll"))
        candidate-files (map (fn [dir] (System.IO.Path/Combine dir assembly-file-name))
                             dirs)
        files (filter (fn [file-name] (System.IO.File/Exists file-name))
                      candidate-files)
        first-choice (first files)]
    (if first-choice
      (System.Reflection.Assembly/LoadFile (System.IO.Path/GetFullPath first-choice))
      (if (not provided-dll)
        (compile (symbol assembly-name))))))

(defn quit
  "Shorthand for (Environment/Exit)."
  ([] (Environment/Exit 0))
  ([exit-code] (Environment/Exit exit-code)))