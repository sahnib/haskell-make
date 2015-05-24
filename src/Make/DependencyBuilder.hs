-- File: DependencyBuilder.hs
-- Builds the dependencies for the target by looking at the timestamps.
module Make.DependencyBuilder
(
  ruleForTarget,
  execute,
  hasExpired,
  isOld
) where 

import Make.Primitives
import System.Cmd
import System.Exit
import System.Directory
import System.Posix.Files

ruleForTarget :: Target -> [Rule] -> Maybe Rule
ruleForTarget t [] = Nothing
ruleForTarget t (x:xs) =
  if (t == target x)
  then Just x
  else ruleForTarget t xs

runCommands :: [String] -> IO ExitCode
runCommands [] = return ExitSuccess
runCommands (x:xs) =
  do
    exitCode <- system x
    case exitCode of ExitSuccess -> runCommands xs
                     ExitFailure a -> return $ ExitFailure a

isOld :: String -> [String] -> IO Bool
isOld targetFile sourcesFiles =
  do
    targetFS <- getFileStatus targetFile
    sourcesFS <- sequence $ (map getFileStatus sourcesFiles)
    let t1 = modificationTime targetFS
    let t2 = map modificationTime sourcesFS
    let retVal = (or $ map (>t1) t2)
    return retVal

-- Returns a true if target is older than the sources
hasExpired :: Rule -> IO Bool
hasExpired rule =
  do
    targetPresent <- doesFileExist $ target rule
    sourcesPresentList <- sequence $ map doesFileExist (sources rule)
    let allSourcesPresent = and sourcesPresentList
    if (targetPresent && allSourcesPresent) then
      (isOld (target rule) (sources rule))
    else
      return True

build :: Rule -> [Rule] -> IO ExitCode
build rule ruleList =
  do
    exitCode <-  executeAll ruleList (sources rule)
    rebuild <- hasExpired rule
-- check here the timestamps of the sources as to if they are more recent than the target
    if (rebuild) then
      case exitCode of ExitSuccess -> runCommands (commands rule)
                       ExitFailure a -> return $ ExitFailure a
    else
      return ExitSuccess

executeAll :: [Rule] -> [Target] -> IO ExitCode
executeAll _ [] = return ExitSuccess
executeAll ruleList (target:otherTargets) =
  do
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
