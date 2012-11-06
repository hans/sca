module Conlang.SoundChangeApplier where

import Data.String.Utils (replace)
import Text.Regex (mkRegex, subRegex)
import Text.Regex.Posix

-- A string of phonemes
type Phoneme = Char

-- Mapping from phoneme class label to phonemes
type PhonemeClass = (Char, [Phoneme])

-- Rule for matching a phoneme or phoneme class within a context
type Pattern = String

data Rule =
     Rule { matchPattern :: Pattern,
            replacement  :: String }

-- Replace any occurrences of a phoneme class label with its respective
-- regular expression character class.
qualifyPatternForClass :: Pattern -> PhonemeClass -> Pattern
qualifyPatternForClass input (k, phonemes) = let reClass = ")[" ++ phonemes ++ "]("
                                             in replace [k] reClass input

-- Qualify a user-defined pattern (i.e., replace custom character
-- classes with literals)
qualifyPattern :: Pattern -> [PhonemeClass] -> Pattern
qualifyPattern = foldl qualifyPatternForClass

applyChange :: String -> Rule -> String
applyChange s (Rule p r) = subRegex (mkRegex p) r s

applyChanges :: String -> [Rule] -> String
applyChanges = foldl applyChange