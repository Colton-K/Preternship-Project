import os # os.system(command)
import argparse

parser = argparse.ArgumentParser()
parser.add_argument('-f', dest='file', type=str, help='.3in file to convert to markdown')
args = parser.parse_args()

# writes the lines list to the file
def writeLines(lines, filename):
  # print them
  # for line in lines:
  #   print(line, end="")
  # print()

  # write them
  with open(filename, "w") as fh:
    for line in lines:
      fh.write(line)

  print("Wrote file:",filename)

# helper for adjustMarkdown
def allUpper(line):
  line = line.rstrip()

  # check if line is empty, if so, return false
  if len(line) == 0:
    return False

  # nromal operation
  for letter in line:
    if not letter.isupper() and letter != " ":
      return False

  return True

# helper for adjustMarkdown
def startOfCodeBlock(line):
  if 'C' in line:
    return "```c\n"
  elif 'Fortran' in line:
    return "```fortran\n"

# reads a markdown file and calls helper function processLine to process the markdown file further
def adjustMarkdown(filename):
  workingLines = []
  newLines = []

  with open(filename, "r") as fh:
    for line in fh.readlines():
      workingLines.append(line)

  inCodeBlock = False
  addText = False
  for i in range(1, len(workingLines)):
    line = ""

    # titles
    if "====" in workingLines[i]:
      if (inCodeBlock):
        newLines.append("```\n")
        inCodeBlock = False

      addText = False

      # if all caps, then heading 1
      if allUpper(workingLines[i-1]):
        if workingLines[i-1] != "NAME\n":
          line = '\n# ' + workingLines[i-1].title()
        else:
          line = '# ' + workingLines[i-1].title()
      # else, heading 2
      else:
        line = '## ' + workingLines[i-1].title()

    # indented blocks
    elif "    " in workingLines[i]:
      # start code block
      inCodeBlock = True
      if len(newLines) > 1:
        if "##" in newLines[len(newLines)-1]:
          newLines.append(startOfCodeBlock(newLines[len(newLines)-1]))
          line = workingLines[i][4:]
        else:
          line = workingLines[i][4:]
      else:
        print("HERE")
        line = "-----------HERE----------------"

    # non-indented blocks
    # check to make sure not going out of bounds
    elif i + 2 < len(workingLines):
      # get name at beginning
      if "**" in workingLines[i]:
        line += "`"
        for letter in workingLines[i]:
          if letter != "*":
            line += letter
        line += "`"

      # handle ':' sections
      elif workingLines[i+2][0] == ':':
        line += '`'
        line += workingLines[i].rstrip()
        line += '`'
        line += ' : '
        line += workingLines[i+2][4:]

      # text blocks below description and errors
      elif len(newLines)>2:
        if 'Errors' in newLines[len(newLines)-1]:
          addText = True
        elif 'Description' in newLines[len(newLines)-1]:
          addText = True
        elif 'See Also' in newLines[len(newLines)-1]:
          addText = True

        # filter headers and blank lines
        if addText and not allUpper(workingLines[i]):
          # create see also links
          if workingLines[i][len(workingLines[i]) - 2] == '\\':
            # Format: [`MPI_Bcast`(3)](MPI_Bcast.html)
            line = "[`{}`(3)]({}.html)\n".format(workingLines[i][:-2],workingLines[i][:-2])

          # normal text
          else:
            line = workingLines[i]

    # print(line)
    if(line):
      newLines.append(line)

  return newLines

def runPandoc(file):
  execLine = "pandoc {} -f man -t markdown -s -o {}".format(file, file[:-3]+"md")
  print("Running:", execLine)
  os.system(execLine)


'''
  Calls all methods to convert from .3in to md
'''
def convert(nroffFilename):
  mdFilename = nroffFilename[:-3]+"md"

  runPandoc(nroffFilename)
  lines = adjustMarkdown(mdFilename)
  writeLines(lines, mdFilename)


convert(args.file)
