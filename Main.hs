import Types
import GrammarParser
import System.Environment

main :: IO ()
main = do
       [inputPath, outputPath] <- getArgs
       fileContent <- readFile inputPath
       writeFile outputPath $grammarToString $stringToGrammar fileContent
       print $stringToGrammar fileContent


----Only one arg: filePath for reading
--main :: IO ()
--main = do 
--       run getArgs
--
--run :: [String] -> IO()
--run [inputPath] = print $stringToGrammar $readFile inputPath
--run [inputPath, outputPath] = writeFile outputPath $grammarToString $stringToGrammar $readFile inputPath
--run _ = error "A program futtatasahoz legalabb egy argumentum szukseges. Annak pedig a beolvasando fajl eleresi cimenek kell lennie.";
