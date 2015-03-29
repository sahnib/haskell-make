import Make.Parser
import Make.Primitives
import System.IO
import Text.ParserCombinators.Parsec

main :: IO()
main =
  do handle <- openFile "Makefile" ReadMode
     contents <- hGetContents handle
     putStrLn $ show $ parse makefile "make::" contents
     hClose handle
