module GrammarParser (parseGrammar) where

import Types
import Data.List.Split

parseGrammar :: String -> Grammar
parseGrammar fileContent = (s, e, ns, ts, rs) --fileContent == "S;epszilon;A,B,C,D,S;a,b,epszilon\nS->a,A|a,D|b,C|epszilon\nA->a,B|b,A|b,C"
  where
    lines = splitOn "\n" fileContent -- lines == [ "S;epszilon;A,B,C,D,S;a,b,epszilon", "S->a,A|a,D|b,C|epszilon", "A->a,B|b,A|b,C" ]
    [s,e,n,t] = splitOn ";" $ head lines --s == "S", e == "epszilon", n == "A,B,C,D,S", t == "a,b,epszilon"
    ns = splitOn "," n --ns == [ "A", "B", "C", "D", "S" ]
    ts = splitOn "," t --ts == [ "a", "b", "epszilon" ]
    ruleLineStrings = tail lines --ruleLineStrings == [ "S->a,A|a,D|b,C|epszilon", "A->a,B|b,A|b,C" ,... ]
    rs = map parseRuleString $tail lines --rs == [("S", [ ["a", "A"], ["a", "D"], ["b", "C"], ["epszilon"] ]), ("A", [ ["a", "B"], ["b", "A"], ["b", "C"] ])]

parseRuleString :: String -> Rule--"S->a,A|a,D|b,C|epszilon" -> ("S", [ ["a", "A"], ["a", "D"], ["b", "C"], ["epszilon"] ] )
parseRuleString line = (s, words)
  where
    [s, wordString] = splitOn "->" line      --s == "S", wordString == "a,A|a,D|b,C|epszilon"
    wordStrings = splitOn "|" wordString     --WordString == ["a,A", "a,D", "b,C", "epszilon"]
    words = map parseWordString wordStrings  --words == [ ["a", "A"], ["a", "D"], ["b", "C"], ["epszilon"] ] :: [WorD]

parseWordString :: String -> WorD                    --Eg: "a,A" -> ["a", "A"]
parseWordString wordString = splitOn "," wordString



--STARTSYMBOL;EMPTYSYMBOL;NONTERMINALS;TERMINALS\n   <-- separated by commas
--NONTERMINAL->SYMBOLS|SYMBOLS|SYMBOLS\n
--NONTERMINAL->SYMBOLS|SYMBOLS|SYMBOLS\n
--etc...

{- Possible input:
S;epszilon;A,B,C,D,S;a,b,epszilon
S->a,A|a,D|b,C|epszilon
A->a,B|b,A|b,C
-}

{-Interpreted as: Grammar
("S","epszilon",["A","B","C","D","S"],["a","b","epszilon"],

    [("S",[["a","A"],["a","D"],["b","C"],["epszilon"]]),
    ("A",[["a","B"],["b","A"],["b","C"]])])
-}