REM To copy next to clojure.main.exe. Prepares the 'CLASSPATH' so we can load
REM apocryphal.clojure.contrib code easily.

set clojure.load.path=c:\Temp\clojure-clr-stuff
clojure.main.exe -i user.clj -r
