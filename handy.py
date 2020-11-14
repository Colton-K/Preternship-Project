import sys
import os
import subprocess

directory = os.getcwd()
#Read in the names of the first and last file to convert

run = False

start_i = 0
for filename in os.listdir(directory):
    #Read all files in the current directory that ends with .3in
        if filename.endswith(".md"):
            f = open(filename,'r',encoding = 'utf-8');
            lines = f.readlines()
            for i in range(len(lines)):
                if lines[i] == '# See Also\n':
                    start_i=i
                if (not start_i==0) and i>start_i:
                    lines[i] = lines[i].replace('.html','.md')
                    lines[i] = lines[i].replace('(MPI_','(./?file=MPI_')
            start_i = 0
            f.close()
            f = open(filename,'w',encoding = 'utf-8')
            for line in lines:
                f.write(line)
