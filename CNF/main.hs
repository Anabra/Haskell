import Types
import GrammarParser
import System.Environment
import Cyk

main :: IO ()
main = do
       [inputPath, inputWordsPath, outputPath] <- getArgs
       fileContent <- readFile inputPath
       inputWords <- readFile inputWordsPath
       let words = [ stringToWord w | w <- lines inputWords]
       let parsedGrammar = chomsky $ stringToGrammar fileContent
       let subresult = [ cyk sortedbrules w | w <- words]
       let result = [ isInLanguage w parsedGrammar | w <- words]
       --writeFile outputPath $grammarToString $stringToGrammar fileContent
       print words
       print parsedGrammar
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
