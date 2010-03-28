
# This is a 'snippet extractor' that will be used to include the code
# in the text in a reliable way.
#
# API:
#
# In the comments in the code:
#
# @begin(snippet-name)
# @end(snippet-name)
#
# These are used to delimit a specific snippet of code. The snippet is
# the code between @cs.begin and @cs.end, without empty lines or lines
# that only contain a comment.
#
# In the documentation:
# @aliasdir(dirAlias, directory)
#
# will be used to specify the base directory for the snippets. This
# marker will be removed. The <directory> is relative to the location
# of the documentation file. The <directory> is associated with the
# shorter <dirAlias>.
#
# Examples:
# @aliasdir(parser, ../src/parser)
# @aliasdir(g, ../src/graphics)
#
# @snippet(dirAlias, path/to/file, snippet-name)
#
# will be replaced with the snippet found in the file, between
# @begin() and @end() markers defining the snippet-name. The
# path to the file is constructed by combining the paths of (1) the
# current documentation file, (2) the content of the <dirAlias> (leave
# empty to ignore at this step) and (3) path/to/file.
#
# Examples (first two should be equivalent if the @aliasdir examples
# were used in the same file):
# @snippet(, ../src/graphics/ui.c, initialization)
# @snippet(g, ui.c, initialization)
# @snippet(parser, parser.c, parse-unary)

import re
import os.path
import sys

def extractSnippet(fileName, snippetName):
    f = file(fileName, "r")
    snippet = []
    snippetFound = False
    inSnippet = False
    re_begin = re.compile("@begin\(([^)]*)\)")
    re_end = re.compile("@end\(([^)]*)\)")
    for line in f:
        if not inSnippet:
            mo = re_begin.search(line)
            if mo != None and mo.group(1) == snippetName:
                snippetFound = True
                inSnippet = True
        else:
            mo = re_end.search(line)
            if mo != None and mo.group(1) == snippetName:
                inSnippet = False
                break
            else:
                empty = line.strip() == ""
                onlyComments = line.lstrip()[:2] == "//"
                if not empty and not onlyComments:
                    snippet.append(line)
    f.close()
    if not snippetFound:
        raise "Snippet %s not found." % (snippetName,)
    else:
        return snippet

def getMinIndent(snippet):
    re_spaces = re.compile("^\s*")
    minIndent = None
    for line in snippet:
        mo_spaces = re_spaces.search(line)
        lineIndent = len(mo_spaces.group(0))
        if minIndent == None or minIndent > lineIndent:
            minIndent = lineIndent
    return minIndent

def indent(snippet, indent):
    minIndent = getMinIndent(snippet)
    if minIndent < indent:
        return [" " * (indent - minIndent) + line for line in snippet]
    elif minIndent > indent:
        return [line[minIndent - indent:] for line in snippet]
    else:
        return snippet

def processFile(fileName):
    f = file(fileName, "r")
    processedContent = []
    docFilePath = os.path.dirname(os.path.abspath(fileName))
    aliasdir = {}
    re_aliasdir = re.compile("\s*@aliasdir\(([^,]*),\s*([^)]*)\)")
    re_snippet = re.compile("\s*@snippet\(([^,]*),\s*([^,]*),\s*([^)]*)\)")
    for line in f:
        mo_aliasdir = re_aliasdir.search(line)
        mo_snippet = re_snippet.search(line)
        if mo_aliasdir != None:
            alias = mo_aliasdir.group(1)
            directory = mo_aliasdir.group(2)
            aliasdir[alias] = directory
        elif mo_snippet != None:
            aliasDir = mo_snippet.group(1)
            if aliasDir == "":
                filePath = os.path.join(docFilePath, mo_snippet.group(2))
            else:
                if not aliasdir.has_key(aliasDir):
                    raise "Directory alias '%s' not defined." % (aliasDir)
                filePath = os.path.join(docFilePath, aliasdir[aliasDir], mo_snippet.group(2))
            snippetName = mo_snippet.group(3)
            snippet = extractSnippet(filePath, snippetName)
            processedContent.extend(indent(snippet, 4))
        else:
            processedContent.append(line)
    f.close()
    return processedContent

if __name__ == "__main__":
    if len(sys.argv) != 3:
        print "usage: %s <fileToProcess> <outputFile>" % (sys.argv[0],)
    else:
        fileName = sys.argv[1]
        f = file(sys.argv[2], "w")
        f.write("".join(processFile(fileName)))
        f.close()
