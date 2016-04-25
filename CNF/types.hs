module Types where

type Symbol = [Char] --a whole string can represent a single symbol
type WorD = [Symbol]
type Rule = (Symbol,[WorD]) --only 2nd type grammar is appropriate /// must give all right hand sides for a rule
type Grammar = (Symbol, Symbol, [Symbol], [Symbol], [Rule]) --(S, epsilon, N, T, R)