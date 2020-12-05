#!/usr/bin/python3
import os

os.system("lex scanner.lex")
os.system("yacc parser.y")
os.system("gcc -g y.tab.c -o test -ll -Ly")

for fname in (os.path.basename(entry) for entry in os.scandir(os.getcwd())):
    if fname.endswith(".txt") and fname.startswith("test"):
        rname = fname.replace("test", "result")
        os.system(f"./test < {fname} > {rname}")
