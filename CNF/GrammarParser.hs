module GrammarParser (stringToGrammar, grammarToString) where

import Types
import Data.List
import Data.List.Split


-------------------------------Decode Grammar from String-------------------------------

stringToGrammar :: String -> Grammar
stringToGrammar fileContent = (s, e, ns, ts, rs)
  where
    lines = splitOn "\n" fileContent
    [s,e,n,t] = splitOn ";" $ head lines
    ns = splitOn "," n
    ts = splitOn "," t
    ruleLineStrings = tail lines
    rs = map stringToRule $tail lines

stringToRule :: String -> Rule
stringToRule line = (s, words)
  where
    [s, wordString] = splitOn "->" line
    wordStrings = splitOn "|" wordString
    words = map stringToWord wordStrings

stringToWord :: String -> WorD
stringToWord wordString = splitOn "," wordString


-------------------------------Encode Grammar to String-------------------------------

grammarToString :: Grammar -> String
grammarToString (s, e, ns, ts, rs) = intercalate ";" [ s, e, ns', ts' ] ++ "\n" ++ rs'
  where
    ns' = wordToString ns
    ts' = wordToString ts
    rs' = init $unlines $map ruleToString rs

ruleToString :: Rule -> String
ruleToString (s, words) = s ++ "->" ++ intercalate "|" ( map wordToString words )

wordToString :: [Symbol] -> String
wordToString ss = intercalate "," ss


--Important!

--Format of the input.
--The format of the content is realy strict, so don't miss anything.
--The fist line contains the start symbol, empty word symbol nonterminal symbols and the terminal symbols.
--As you can see, after the start and empty symbols there is a semicolon. It is also true for nonterminals and terminals,
--but the nonterminals and terminals are separated by a simple colon.
--At the end of the line there is no sperarator. (It is also true for all lines, even the last line, becouse that line does not end with new line tag. (\n))
--The rules of the grammar are in the next lines (after the first line), in the following format:
--Every line starts with the start symbol of the rule followed by "->" sign and the list of words.
--Each word separated by "|" sign, and if the word contains multiple symbols, than those are separated by colon from each other.
--As you can see, these lines end only with new line symbols (\n), except the last line.


--Common mistakes:
--First line semicolon, colon mismatch,...
--Line endings.
--No new line signs at the end of the rule line (except last line)


{- Possible input:
S;epszilon;A,B,C,D,S;a,b,epszilon
S->a,A|a,D|b,C|epszilon
A->a,B|b,A|b,C
-}
