import System.IO
import System.Cmd
import System.Exit

-- Represents the source of the rule, 
-- a.k.a. the dependencies of the target which has to be built by the rule
data Source = StrSource String | RuleSource Rule deriving (Show)

-- data structure representing the complete rule, 
-- has a target to be built and the sources, a.k.a. the dependencies from which to build it
data Rule = Rule {target:: String, sources:: [Source], commands:: [String]} deriving (Show)

type Target = String

-- the whole contents of the "Makefile", as a list of lines.
type Contents = [String]

main :: IO()
main = do
     handle <- openFile "Makefile" ReadMode
     contents <- hGetContents handle
     let rule = buildDependencyTree (lines contents) "monte_pi_sprng"
     exitCode <- executeRule rule
     hClose handle

listTargets :: Contents -> [String]
listTargets xs = map (\(x:":":ys) ->x ) (filter isARule $ map words xs)

-- takes the contents of the "Makefile" and builds the dependency tree, finally returning the Rule for the target specified
buildDependencyTree :: Contents -> Target -> Rule
buildDependencyTree contents target = buildDependencyTreeFromRule (head (filter (isRuleForTarget target) $ map words contents)) contents

-- Returns true if the line contains the rule to build the target specified
isRuleForTarget :: Target -> [String] -> Bool
isRuleForTarget t (x:":":xs) = x == t
isRuleForTarget t (x:xs) = 
    if (last x == ':')
        then (x == t)
        else False
isRuleForTarget t ys = False

isACommand :: String -> Bool
isACommand [] = False
isACommand ('\t':xs) = True
isACommand _ = False

isARule :: [String] -> Bool
isARule (_:":":ys) = True
isARule _ = False


buildDependencyTreeFromRule :: [String] -> Contents -> Rule
buildDependencyTreeFromRule (t : ":" : []) contents = 
    Rule {target = t, sources = [], commands = cmds}
    where
        cmds = getCommandsForTarget t contents

buildDependencyTreeFromRule (t : ":" :xs) contents = 
    Rule {target = t, sources = ys, commands = cmds}
    where
        ys = map (\x -> if elem x (listTargets contents) 
            then RuleSource $ buildDependencyTree contents x 
            else StrSource x ) xs
        cmds = getCommandsForTarget t contents

-- Builds all the dependencies for the rule
-- and executes all the commands for the rule afterwards
-- A failure leads to halt in execution and propagates all the way upwards
-- to the user
executeRule :: Rule -> IO ExitCode
executeRule (Rule {target = _, sources = srs, commands = cmds}) = 
    do
    exitCode <- buildSources srs
    if (exitCode == ExitSuccess) 
        then
            executeCommands cmds
        else
            return exitCode

getCommandsForTarget :: String -> Contents -> [String]
getCommandsForTarget target contents = mylast $ foldl foo (target, False, []) contents 

mylast :: (Target, Bool, [String]) -> [String]
mylast (_, _, xs) = xs

foo :: (Target, Bool, [String]) -> String -> (Target, Bool, [String])
foo (target, ruleStarted, commands) line =  
    if (ruleStarted) 
    then 
        if (isACommand line) 
        then 
            (target, ruleStarted, commands ++ [line])
        else
            (target, False, commands)
    else
        if (isRuleForTarget target $ words line) 
            then
                (target, True, commands)
            else
                (target, False, commands)


executeCommands :: [String] -> IO ExitCode
executeCommands [] = returcuteCommands (command:xs) = 
    do
        exitCode <- system command
        case exitCode of (ExitFailure _) -> return exitCode
                          _              -> executeCommands xs


buildSources :: [Source] -> IO ExitCode
buildSources [] = return ExitSuccess
buildSources ( (RuleSource rule) : xs) = 
    do
        exitCode <- executeRule rule
        case exitCode of (ExitFailure _) -> return exitCode
                         _               -> buildSources xs

buildSources ( (StrSource source) : xs) = buildSources xs
