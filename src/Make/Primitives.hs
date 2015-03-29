module Make.Primitives
(
   Rule(..)
 , Target
 , Source
   
 ) where


 -- Represents the source of the rule, 
-- a.k.a. the dependencies of the target which has to be built by the rule
type Source = String

-- data structure representing the complete rule, 
-- has a target to be built and the sources, a.k.a. the dependencies from which to build it
data Rule = Rule {target:: String, sources:: [Source], commands:: [String]} deriving (Show)

type Target = String
