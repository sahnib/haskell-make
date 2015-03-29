-- File Parser.hs

module Make.Parser
(
  makefile
)
where

import Text.ParserCombinators.Parsec
import Make.Primitives
 
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
     char ':'
     s <- parseSources
     eol
     cmds <- parseCommands
     spaces
     return Rule {target = t, sources = s, commands = cmds}

--Parses a newLine
eol :: GenParser Char st Char
eol = char '\n'

parseTarget :: GenParser Char st Target
parseTarget =
  many (noneOf ":")

parseSources :: GenParser Char st [Source]
parseSources =
  do
    many (char ' ' >> parseSource)
    <|> (return [])

parseSource :: GenParser Char st Source
parseSource =
  do
    -- Skip Whitespace characters at the beginning
    many (oneOf " \t")
    s <- many (noneOf " \n\t")
    return s

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
