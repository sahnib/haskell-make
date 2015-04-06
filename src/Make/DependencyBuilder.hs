module Make.DependencyBuilder
(
  ruleForTarget,
  execute
) where 

import Make.Primitives
import System.Cmd
import System.Exit

ruleForTarget :: Target -> [Rule] -> Maybe Rule
ruleForTarget t [] = Nothing
ruleForTarget t (x:xs) =
  if (t == target x)
  then Just x
  else ruleForTarget t xs

runCommands :: [String] -> IO ExitCode
runCommands [] = return ExitSuccess
runCommands (x:xs) = do
  exitCode <- system x
  case exitCode of ExitSuccess -> runCommands xs
                   ExitFailure a -> return $ ExitFailure a

build :: Rule -> [Rule] -> IO ExitCode
build rule ruleList = do
  exitCode <-  executeAll ruleList (sources rule)
  case exitCode of ExitSuccess -> runCommands (commands rule)
                   ExitFailure a -> return $ ExitFailure a

executeAll :: [Rule] -> [Target] -> IO ExitCode
executeAll _ [] = return ExitSuccess
executeAll ruleList (target:otherTargets) = do
  exitCode <- execute ruleList target
  case exitCode of ExitSuccess -> executeAll ruleList otherTargets
                   ExitFailure a -> exitWith $ ExitFailure a

execute :: [Rule] -> Target -> IO ExitCode
execute ruleList target =
  -- execute all source rules
  -- see if the target has timestamp earlier than its sources, if yes, execute all its commands
  case rule of Nothing ->  return ExitSuccess
               Just rule -> build rule ruleList
  where
    rule = ruleForTarget target ruleList
