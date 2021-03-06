Chomsky normal form and CYK.
===================

###Short description.
> The program will **convert** the given grammar to **CNF**, and determine whether a word is in the language generated by the grammar, using the *CYK algorythm*. 

###How to run the program:
The program should be run by executing the **Main.exe** file from the command line with **two paramters**. The first one is the *grammar* input file, the second one is the *words* input file.
**The output should be displayed on the console**. It will display the **converted grammar** and a **list of boolean - word pairs** indicating if the word is in the language or not. 

####On Windows:  *command line prompt*
>**\>Main.exe grammar.txt words.txt**

----------

##Format of the documents

###Format of the "grammar" input file:

>**The format of the content is really strict**, so don't miss anything.

>***The first line contains the start symbol, empty word symbol nonterminal symbols and the terminal symbols.***

>##### Possible input of grammar:
>>```
S;epsilon;S,A,B,C;a,b,c
S->A,S|S,B|a
A->B,C|a
B->A,B|C,C|b
C->A,B|c
```

>As you can see, after the start and empty symbols there are ***semicolons***. It is also true for nonterminals and terminals, but the nonterminals and terminals themselves are separated by a ***simple comma***.
At the end of the line there is ***no sperarator***. (It is also true for all lines, even the last line, because that ***line does not end with new line tag***. (\n))
The rules of the grammar should be written from the second line, in the following format:
Every line starts with the left-hand side of the rule followed by ***"->" sign*** and the list of words.
Each word separated by ***"|" sign***, and if the word contains multiple symbols, than those are separated by colon from each other.
As you can see, these lines end only with new line symbols (\n), ***except the last line.***

####One important thing is that ***a whole string can represent a single symbol***. That's why you have to separate the symbols with commas. 

-------------------

###Format of the "words" input file:

>Each line should contain a single word, consisting of the any number of symbols
>The symbols within a word should be separated by ***commas***.
>Each line should have a ***newline character*** at the end of it, **except the last one**.

>##### Possible input of words:
>>```
a
b
c
a,a
a,b
b,b
b,c
c,c
a,a,b
a,b,b
b,b,c
b,c,c
a,a,b,b
a,b,b,c
b,b,c,c
a,a,b,b,c
a,b,b,c,c
a,a,b,b,c,c
```
----------

>##### Expected output on console:
>>```"S;epsilon;S,A,B,C,{a},{b},{c};a,b,c"
"S->A,S|S,B|a"
"A->B,C|a"
"B->A,B|C,C|b"
"C->A,B|c"
"{a}->a"
"{b}->b"
"{c}->c"
[(["a"],True),(["b"],False),(["c"],False),(["a","a"],True),(["a","b"],True),(["b","b"],False),(["b","c"],False),(["c","c"],False),(["a","a","b"],True),(["a","b","b"],True),(["b","b","c"],False),(["b","c","c"],False),(["a","a","b","b"],True),(["a","b","b","c"],False),(["b","b","c","c"],False),(["a","a","b","b","c"],False),(["a","b","b","c","c"],True),(["a","a","b","b","c","c"],True)]
```
----------

###Common mistakes:
- First line semicolon, colon mismatch,...
- Line endings.
- No new line signs at the end of the rule line (except last line)
