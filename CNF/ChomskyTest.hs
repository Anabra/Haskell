module ChomskyTest where

import Types


terminals1 = ["a","b","epsilon"] :: [Symbol]
nonterminals1 = ["S","A","B","C","D"] :: [Symbol]
rules1 = [("S", [["A"], ["B"],       ["A","B","B","B"], ["C"]])
         ,("A", [["a"], ["a","B"],   ["a","A","b","B"]])
         ,("B", [["b"], ["epsilon"], ["A","b","B"]])
         ,("C", [["a"], ["a","b"],   ["A"],["D"]])
         ,("D", [["D"], ["D","D"]])] :: [Rule]
grammar1 = ("S", "epsilon", nonterminals1, terminals1, rules1) :: Grammar


terminals2 = ["a","b","c","d","epsilon"] :: [Symbol]
nonterminals2 = ["S","A","B","C","D"] :: [Symbol]
rules2 = [("S", [["A"], ["B"],       ["A","B","B","B"], ["C"]])
         ,("A", [["a"], ["a","B"],   ["a","A","b","B"]])
         ,("B", [["c"], ["A","C"],   ["D","C"]])
         ,("C", [["a"], ["a","b"],   ["A"],["D"]])
         ,("D", [["D"], ["D","D"],   ["d"]])] :: [Rule]
grammar2 = ("S", "epsilon", nonterminals2, terminals2, rules2) :: Grammar

terminals3 = ["a","b","epsilon"] :: [Symbol]
nonterminals3 = ["S"] :: [Symbol]
rules3 = [("S", [["a","S","b"], ["b","S","a"], ["S","S"], ["epsilon"] ])] :: [Rule]
grammar3 = ("S", "epsilon", nonterminals3, terminals3, rules3) :: Grammar