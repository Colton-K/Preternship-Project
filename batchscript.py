import sys
import os
import subprocess

directory = os.getcwd()
#Read in the names of the first and last file to convert
start = input('Please enter the name of the first .3in file to convert:  ')
end = input('Please enter the name of the last .3in file to convert:  ')
run = False

for filename in os.listdir(directory):
    #Read all files in the current directory that ends with .3in
        if filename.endswith(".3in"):
            if(filename==start):
                run = True
            if(run):
                #change the command to 'python3 ...' if you cannot run it
                command = 'python conversion.py -f' + filename
                subprocess.run(command,shell=True)
            if filename==end:
                break
