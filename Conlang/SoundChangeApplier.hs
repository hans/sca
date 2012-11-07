module Conlang.SoundChangeApplier where

import Data.String.Utils (replace)

-- A string of phonemes
type Phoneme = Char

-- A pair of phoneme class and corresponding phonemes
type PhonemeClass = (Char, [Phoneme])

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

applyRule :: [PhonemeClass] -> String -> Rule -> String

applyRule classes (c:cs) r@(PhonemeRule l ps _ _)
          | c == l     = ps ++ applyRule classes cs r
          | otherwise  = c : applyRule classes cs r

applyRule _ "" _ = ""

applyRules :: [PhonemeClass] -> String -> [Rule] -> String
applyRules classes = foldl (applyRule classes)

matchContext :: Context -> String -> Bool
matchContext = (==)