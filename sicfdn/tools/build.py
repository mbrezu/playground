
# This is the documentation builder.
#
# It will scan all the files in ../doc for .mds, process them using
# the snippet extractor, then output files in ../html (which it recreates).
#
# It expects to be run from cygwin and have Markdown.pl on the path.

import glob
import snipext
import sys
import os.path

def replaceExtension(fileName, extension):
    pos = fileName.rfind(".")
    if pos == -1:
        return fileName + "." + extension
    else:
        return fileName[:pos] + "." + extension

def writeFile(fileName, lines):
    f = file(fileName, "w")
    f.write("".join(lines))
    f.close()

if __name__ == "__main__":
    docPath = os.path.abspath(os.path.join(os.path.abspath(os.path.dirname(sys.argv[0])), "../doc"))
    htmlPath = os.path.abspath(os.path.join(
        os.path.abspath(os.path.dirname(sys.argv[0])), "../html"))
    os.system("rm -rf " + htmlPath)
    os.system("mkdir " + htmlPath)
    filesToDelete = []
    for fileName in glob.glob(os.path.join(docPath, "*.md")):
        processedContent = snipext.processFile(fileName)
        fileDir = os.path.dirname(fileName)
        fileNameShort = os.path.basename(fileName)
        intermediateFileName = os.path.join(fileDir, replaceExtension(fileNameShort, "tmp"))
        writeFile(intermediateFileName, processedContent)
        filesToDelete.append(intermediateFileName)
        htmlFilePath = os.path.join(htmlPath, replaceExtension(fileNameShort, "html"))
        os.system("Markdown.pl %s >%s" % (intermediateFileName, htmlFilePath))
    os.system("rm " + " ".join(filesToDelete))
