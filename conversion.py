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

#Add appropriate   `` around function names and parameters
def adjustWords(words):
    for index in range(len(words)):
        last_mark = ''
        #check function names
        if '_' in words[index]:
            #Move the punctuation out of ``
            if(words[index][len(words[index])-1].isalnum()==False):
                last_mark = words[index][len(words[index])-1]
                words[index]=words[index][0:len(words[index])-1]
            words[index]='`{}`'.format(words[index])
        #check parameters
        elif words[index][0]=='*' and words[index][len(words[index])-1] == '*':
            if(words[index][len(words[index])-2].isalnum()==False):
                last_mark = words[index][len(words[index])-2]
                words[index]=words[index][0:len(words[index])-2]+words[index][len(words[index])-1:]
            words[index]=words[index].replace('*','`')
        #Delete unnecassary escape signs
        elif '\\' in words[index]:
            words[index]=words[index].replace('\\','')
        words[index]+=last_mark
    line = (' ').join(words)
    return line

# adds newline inside the code block if necessary
def checkBreak(line):
  editedLine = ""
  # check beginning of c
  if "#include" in line:
    editedLine += "\n"
  # check beginning of fortran
  elif "USE MPI" in line:
    editedLine += "\n"
  # check beginning of fortran2008 
  elif "USE mpi_f08" in line:
    editedLine += "\n"
  # check beginning of function in c
  elif " MPI_" in line:
    editedLine += "\n"
  # check beginning of function in both fortrans
  elif "MPI_" in line and not ':' in line:
    editedLine += "\n"


  # add line and return
  editedLine += line
  return editedLine

# reads a markdown file and calls helper function processLine to process the markdown file further
def adjustMarkdown(filename):
  workingLines = []
  newLines = []
  fixedWidthWords = []

  with open(filename, "r") as fh:
    for line in fh.readlines():
      workingLines.append(line)

  inCodeBlock = False
  addText = False
  parameterLine = False
  #check whether it is in the name section
  name = False
  for i in range(1, len(workingLines)):
    line = ""

    #delete unnecassary blank lines
    if workingLines[i].isspace():
        continue
    # titles
    elif "====" in workingLines[i]:
      if (inCodeBlock):
        newLines.append("```\n")
        newLines.append('\n')
        inCodeBlock = False

      addText = False

      # if all caps, then heading 1
      if allUpper(workingLines[i-1]):
        if workingLines[i-1] != "NAME\n":
        #add a new line after each title
          line = '\n# ' + workingLines[i-1].title()+'\n'
          name = False
        else:
          line = '# ' + workingLines[i-1].title()+'\n'
          name = True
      # else, heading 2
      else:
        line = '## ' + workingLines[i-1].title()+'\n'

    # indented blocks
    elif "    " in workingLines[i]:
      # start code block
      inCodeBlock = True
      if len(newLines) > 1:
        if "##" in newLines[len(newLines)-1]:
          newLines.append(startOfCodeBlock(newLines[len(newLines)-1]))
          line = workingLines[i][4:]

        else:
          # line = workingLines[i][4:]
          line = checkBreak(workingLines[i][4:])
          #When changing a new line in a code block, use six spaces instead of a tab
          if(line[0]=='\t'):
              line = '    '+line[1:]
      else:
        print("HERE")
        line = "-----------HERE----------------"

    # non-indented blocks
    # check to make sure not going out of bounds
    elif i + 2 < len(workingLines):
      # get name at beginning
      if "**" in workingLines[i]:
        # line += "`"
        for letter in workingLines[i]:
          if letter != "*":
            line += letter
        # line += "`"

      # handle ':' sections
      elif workingLines[i+2][0] == ':':
        parameterLine = True
        # line += '* `' # ticks will be added later
        line += '* '
        line += workingLines[i].rstrip()
        # line += '`'
        line += ' : '
        line += workingLines[i+2][4:]
        # add word to go through other lines and syntax highlight later
        fixedWidthWords.append(workingLines[i].rstrip())

      # text blocks below description and errors
      elif len(newLines)>2:
        if 'Errors' in newLines[len(newLines)-1]:
          addText = True
        elif 'Description' in newLines[len(newLines)-1]:
          addText = True
        elif 'See Also' in newLines[len(newLines)-1]:
          addText = True
          #add notes section
        elif 'Notes' in newLines[len(newLines)-1]:
          addText = True

        # filter headers and blank lines
        if addText and not allUpper(workingLines[i]):
          # create see also links
          if workingLines[i][len(workingLines[i]) - 2] == '\\': 
            # Format: [`MPI_Bcast`(3)](MPI_Bcast.html)
            line = "[`{}`(3)]({}.html)\n".format(workingLines[i][:-2],workingLines[i][:-2])
          # normal text
          else:
            #Filter function name and parameters in it
            line =  workingLines[i]
            #if a normal text is under name section, also add it to newLines
      elif(name==True and workingLines[i].isupper()==False and workingLines[i]!='\n'):

          line = workingLines[i]


    else:
        line =  workingLines[i]

    # #adjust words for each line
    try:
      # make sure not in a code block
      if not inCodeBlock and not parameterLine:
        line = adjustWords(line.split(' '))
    except:
        #if the line only has one word, skip this line
        pass


    # make things in fixedWidthWords fixed-width font if needed
    if not inCodeBlock and not parameterLine:
      # check if any of the words are in the line
      for word in fixedWidthWords:
        # go through the line
          if word in line:
            line = line.replace(word, '`' + word + '`')


    # finally, add line
    if(line):
      newLines.append(line)
    
    # at the end of the line, reset the line tag for the next iteration
    parameterLine = False

  # add the links in the see also
  maxNumLinks = 10 # how far down the lines do you wanna check?
  for i in range(len(newLines), len(newLines)-maxNumLinks, -1):
    if " " not in newLines[i-1]:
      # newLines[i-1] = "[`{}`(3)]({}.html)\n".format(newLines[i-1][:-2].rstrip(),newLines[i-1][:-2].rstrip()) # how it should be
      newLines[i-1] = "[`{}`(3)]({}.html)\n".format(newLines[i-1][1:-1],newLines[i-1][1:-1]) # how it is because there is a newline added somewhere...


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
