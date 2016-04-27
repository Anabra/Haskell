module Cyk where

import Data.List
import ChomskyTest
import Types
import Chomsky

--for testing
toWord :: [Char] -> WorD
toWord [] = []
toWord (x:xs) = [x] : toWord xs
 
breakdown :: Rule -> [Rule']
breakdown rule@(lhs,rhs) = [ (lhs, subrule) | subrule <- rhs ]

breakdownRules :: Grammar -> [Rule']
breakdownRules grammar = concatMap (breakdown) rules
    where
    (_, _, _, _, rules) = grammar


--the the input lists should be sorted
merge :: (a -> a -> Bool) -> [a] -> [a] -> [a]
merge _ [] list2 = list2
merge _ list1 [] = list1
merge rel list1@(x:xs) list2@(y:ys)
    | rel x y   = x : merge rel xs list2
    | otherwise = y : merge rel list1 ys
    
mergesort :: (a -> a -> Bool) -> [a] -> [a]
mergesort _ [x] = [x]
mergesort rel xs = merge rel (mergesort rel firstHalf) (mergesort rel secondHalf)
    where
    half = length xs `div` 2
    firstHalf = take half xs
    secondHalf = drop half xs


descartes :: [[a]] -> [[a]]
descartes [] = [[]]
descartes (x:xs) = [  a : dx | a <- x, dx <- descartes xs]


sliceWord :: [a] -> [([a], [a])]
sliceWord word@(x:xs) = slcwrdHelper [x] xs
    where
    slcwrdHelper :: [a] -> [a] -> [([a], [a])]
    slcwrdHelper xs [y] = [(xs, [y])]
    slcwrdHelper xs other@(y:ys) = (xs, other) : slcwrdHelper (xs ++ [y]) ys
    --slcwrdHelper [x] ys = [([x], ys)]
    --slcwrdHelper xs ys = (xs, ys) : slcwrdHelper (init xs) (last xs : ys)
    
--searchForWord a b == c // a -> rhs to search for; b -> rules to filter (sorted by rhs); c -> lhs-s of each rule that has passed the query
searchForWord :: WorD -> [Rule'] -> [Symbol]
searchForWord _ [] = []
searchForWord word rules@(r:rs)
    | word < rhs = []
    | word > rhs = searchForWord word rs
    | otherwise = lhs : searchForWord word rs
    where (lhs, rhs) = r

sortedbrules1 = mergesort (\(_,a) (_,b) -> a < b) $ breakdownRules $ chomsky grammar1
sortedbrules2 = mergesort (\(_,a) (_,b) -> a < b) $ breakdownRules $ chomsky grammar2
sortedbrules3 = mergesort (\(_,a) (_,b) -> a < b) $ breakdownRules $ chomsky grammar3
    

cyk :: [Rule'] -> WorD -> [Symbol]
cyk rules [t] = searchForWord [t] rules
cyk rules word = concatMap (flip (searchForWord) rules) descartesWords
    where
    descartesWords = concat [ descartes [cyk rules pre, cyk rules suf] | w@(pre,suf) <- sliceWord word]
    
    --searchForWord ["a"] ( mergesort (\(_,a) (_,b) -> a < b) (breakdownRules (chomsky grammar1)))
    
isInLanguage :: WorD -> Grammar -> Bool
isInLanguage word grammar = start `elem` (cyk rules' word)
    where
    rules' = mergesort (\(_,a) (_,b) -> a < b) $ breakdownRules $ cgrammar
    (start, eps, nonterminals, terminals, rules) = cgrammar
    cgrammar = chomsky grammar