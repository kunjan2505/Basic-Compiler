# Basic-Compiler

1. Put lex.l, parser.y, input.txt in same directory. 
2. Execute command: bison -d parser.y
3. Execute command: flex lex.l
4. Execute command: gcc parser.tab.c lex.yy.c -ly -lfl (It may give error in wsl environment. In that case remove flag -ly and then execute rest of the command.)
5. Execute command: ./a.out < input.txt
6. Output file “out.ll” will be generated in the same directory.
