import Types
import GrammarParser
import System.Environment

--Only one arg: filePath for reading
main :: IO ()
main = do
       [inputPath] <- getArgs
       fileContent <- readFile inputPath
       print $parseGrammar fileContent