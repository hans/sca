module Conlang.SoundChangeApplier where

import Data.String.Utils (replace)

-- A string of phonemes
type Phoneme = Char

-- Matching context (combination of phonemes and phoneme classes)
type Context = String

-- Rule for matching a phoneme or phoneme class within a context
data Rule =
     Rule { matchChar     :: Char,
            replacement   :: [Phoneme],
            beforeContext :: [Phoneme],
            afterContext  :: [Phoneme] }

applyRule :: String -> Rule -> String
applyRule (c:cs) r@(Rule l ps _ _) | c == l     = ps ++ applyRule cs r
                                   | otherwise = c : applyRule cs r
applyRule ""     _                             = ""

applyRules :: String -> [Rule] -> String
applyRules = foldl applyRule

matchContext :: Context -> String -> Bool
matchContext = (==)