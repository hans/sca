module Conlang.SoundChangeApplier where

import Data.Map hiding (foldl)
import Data.String.Utils (replace)

-- A string of phonemes
type Phoneme = Char

-- A mapping from phoneme class to corresponding phonemes
type PhonemeClassMap = Map Char [Phoneme]

-- Matching context (combination of phonemes and phoneme classes)
type Context = String

-- Rule for matching a phoneme or phoneme class within a context
data Rule =
       PhonemeRule { matchPhoneme  :: Phoneme,
                     replacement   :: [Phoneme],
                     beforeContext :: Context,
                     afterContext :: Context }
     | PhonemeClassRule { matchChar     :: Char,
                          replacement   :: [Phoneme],
                          beforeContext :: [Phoneme],
                          afterContext  :: [Phoneme] }

-- Rule application

applyRule :: PhonemeClassMap -> String -> Rule -> String

applyRule classes (c:cs) r@(PhonemeClassRule l ps bc ac)
          | c `elem` (classes ! l) = ps ++ applyRule classes cs r
          | otherwise              = c : applyRule classes cs r

applyRule classes (c:cs) r@(PhonemeRule l ps _ _)
          | c == l     = ps ++ applyRule classes cs r
          | otherwise  = c : applyRule classes cs r

applyRule _ "" _ = ""

applyRules :: PhonemeClassMap -> String -> [Rule] -> String
applyRules classes = foldl (applyRule classes)

matchContext :: Context -> String -> Bool
matchContext = (==)