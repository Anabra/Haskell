import Types
import GrammarParser
import System.Environment
import Control.Monad
import Chomsky
import Cyk

main :: IO ()
main = do
       [inputPath, inputWordsPath] <- getArgs
       fileContent <- readFile inputPath
       inputWords <- readFile inputWordsPath
       let words = [ stringToWord w | w <- lines inputWords]
       let parsedGrammar = chomsky $ stringToGrammar fileContent
       let result = [ (w ,isInLanguage w parsedGrammar) | w <- words]
       mapM (print) (lines $ grammarToString parsedGrammar)
       print result


----Only one arg: filePath for reading
--main :: IO ()
--main = do 
--       run getArgs
--
--run :: [String] -> IO()
--run [inputPath] = print $stringToGrammar $readFile inputPath
--run [inputPath, outputPath] = writeFile outputPath $grammarToString $stringToGrammar $readFile inputPath
--run _ = error "A program futtatasahoz legalabb egy argumentum szukseges. Annak pedig a beolvasando fajl eleresi cimenek kell lennie.";
