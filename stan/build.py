#!/usr/bin/env python

import sys
from memoize import memoize, set_force_build

def run(cmd):
    status = memoize(cmd)
    if status: sys.exit(status)

def forceRun(cmd):
    set_force_build(True)
    status = memoize(cmd)
    set_force_build(False)
    if status: sys.exit(status)

modules = ["parserTypes",
           "utils",
           "parser",
           "sem",
           "parserTests"]

packages = ["oUnit"]

def tests(compiler, object_extension):
    global modules
    global packages
    package_list = ",".join(packages)
    run("mkdir -p bin/tests")
    for module in modules:
        run("ocamlfind %s -c -thread -package %s -I src src/%s.ml" % (compiler,
                                                                      package_list,
                                                                      module) )
    object_list = " ".join(["src/%s.%s" % (module, object_extension)
                            for module in modules])
    run("ocamlfind %s -thread -o bin/tests/test -linkpkg -package %s %s" % (compiler,
                                                                            package_list,
                                                                            object_list))
    forceRun("bin/tests/test")

def clean():
    forceRun("rm -rf bin")
    forceRun("rm src/*.cmo src/*.cmi src/*.o src/*.cmx")

if len(sys.argv) < 2:
    print "usage: ./build.py clean"
    print "usage: ./build.py tests"
    print "usage: ./build.py testsopt"
elif sys.argv[1] == "clean":
    clean()
elif sys.argv[1] == "tests":
    tests("ocamlc", "cmo")
elif sys.argv[1] == "testsopt":
    tests("ocamlopt", "cmx")
else:
    print "unknown command " + sys.argv[1]
