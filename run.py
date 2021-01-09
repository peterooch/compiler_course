#!/usr/bin/python3
import os

os.system("lex scanner.l")
os.system("yacc parser.y")
os.system("gcc -g y.tab.c analyzer.c -o test -ll -Ly")

output_file = open("doc.txt", "w")
for fname in (os.path.basename(entry) for entry in os.scandir(os.getcwd())):
    if fname.endswith(".txt") and fname.startswith("test"):
        rname = fname.replace("test", "result")
        output_file.write(f"Input file: ({fname})\n")
        #with open(fname,"r") as input_file:
        #    output_file.write(f"{input_file.read()}\n")
        os.system(f"./test < {fname} > {rname}")
        #with open(rname,"r") as input_file:
        #    output_file.write(f"Output Message: {input_file.read()}\n")
