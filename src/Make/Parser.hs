-- File Parser.hs

module Make.Parser
(
  makefile
)
where

import Text.ParserCombinators.Parsec
import Make.Primitives

-- 
-- Returns the complete list of Rules
makefile :: GenParser Char st [Rule]
makefile =
  do result <- many parseRule
     eof
     return result

-- Parses a Rule, separately parses the target, source and the commands and then builds the object
parseRule :: GenParser Char st Rule
parseRule =
  do t <- parseTarget
     s <- parseSources
     eol
     cmds <- parseCommands
     return Rule {target = t, sources = s, commands = cmds}

isSpace :: Char -> Bool
isSpace ' ' = True
isSpace '\t' = True
isSpace _ = False

--Parses a newLine
eol :: GenParser Char st Char
eol = char '\n'

parseTarget :: GenParser Char st Target
parseTarget =
  many (noneOf ":\n")

parseSources :: GenParser Char st [Source]
parseSources =
  do result <- many (satisfy(\c -> isSpace c) >> parseSource)
     eol
     return result

parseSource :: GenParser Char st Source
parseSource =
  many alphaNum

parseCommands :: GenParser Char st [String]
parseCommands =
  do
    -- A command has to start with a tab character to be included in this rule
    -- else we return an empty list
    many (char '\t' >> parseCommand)
    <|> (return [])

parseCommand :: GenParser Char st String
parseCommand =
  many (noneOf "\n")
