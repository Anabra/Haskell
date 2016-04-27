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

terminals4 = ["a","b"]
nonterminals4 = ["S", "A", "B", "C", "D"]
rules4 = [("S",[["A","B"],["C","D"]]),("A",[["A","A"],["C","S"],["a"]]),("B",[["B","B"],["D","S"],["b"]]),("C",[["D","A"],["C","B"],["a"]]),("D",[["D","D"],["b"]])]
grammar4 = ("S", "epsilon", nonterminals4, terminals4, rules4) :: Grammar

terminals5 = ["a","b","c"]
nonterminals5 = ["S", "A", "B", "C"]
rules5 = [("S",[["A","B"],["S","C"]]),("A",[["A","C"],["a"],["c"]]),("B",[["B","C"],["b"],["c"]]),("C",[["C","S"],["S","S"],["c"]])]
grammar5 = ("S", "epsilon", nonterminals5, terminals5, rules5) :: Grammar

terminals6 = ["a","b","c"]
nonterminals6 = ["S","A","B","C"]
rules6 = [("S",[["A","S"],["S","B"],["a"]]),("A",[["B","C"],["a"]]),("B",[["A","B"],["C","C"],["b"]]),("C",[["A","B"],["c"]])]
grammar6 = ("S", "epsilon", nonterminals6, terminals6, rules6) :: Grammar







