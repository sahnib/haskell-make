import Make.Parser
import Make.Primitives
import System.IO
import Text.ParserCombinators.Parsec
import Make.DependencyBuilder

main :: IO()
main =
  do handle <- openFile "Makefile" ReadMode
     contents <- hGetContents handle
     case (parse makefile "make::" contents) of
      Left msg -> putStrLn $ show msg
      Right rules -> do
        exitCode <- execute rules "all"
        putStrLn $ show exitCode
     hClose handle
