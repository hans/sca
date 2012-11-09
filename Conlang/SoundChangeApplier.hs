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
       PhonemeRule      { matchPhoneme  :: Phoneme,
                          replacement   :: [Phoneme],
                          beforeContext :: Context,
                          afterContext  :: Context }
     | PhonemeClassRule { matchChar     :: Char,
                          replacement   :: [Phoneme],
                          beforeContext :: Context,
                          afterContext  :: Context }

instance Show Rule where
    show (PhonemeRule m r b a)      = m : " > " ++ r ++ " / " ++ b ++ "_" ++ a
    show (PhonemeClassRule m r b a) = m : " > " ++ r ++ " / " ++ b ++ "_" ++ a

-- Rule application with context tracking

takeLast :: Int -> [a] -> [a]
takeLast n xs = drop (length xs - n) xs

-- Context-free rule match. Innermost and simplest logic goes here.
matchRule :: PhonemeClassMap -> Rule -> Char -> Bool

-- A phoneme class rule should match when the given char is a phoneme
-- class label.
matchRule classes (PhonemeClassRule l _ _ _) c = c `elem` (classes ! l)

-- A phoneme rule should match when the given char is equal to the
-- rule's match char.
matchRule _ (PhonemeRule l _ _ _) c = c == l

-- Context-aware rule application. Delegates primary rule logic
-- (context-free) to `matchRule` and context matching to `matchContext`.
applyRule' :: PhonemeClassMap -> Rule -> (Context, String) -> (Context, String)
applyRule' cs r (preceding, c : s)
           = if    matchRule cs r c
                && matchContext cs (beforeContext r) bc
                && matchContext cs (afterContext r) ac
             then (preceding ++ replacement r, s)
             else (preceding ++ [c], s)
             where
                -- Trim before-contexts and after-contexts to match what
                -- is needed by the rule
                bc = takeLast (length (beforeContext r)) preceding
                ac = take (length (afterContext r)) s

applyRule' _ _ (x, "") = (x, "")

applyRule :: PhonemeClassMap -> String -> Rule -> String
applyRule classes s r = fst . head $ dropWhile (\x -> snd x /= "")
                                     (iterate (applyRule' classes r) ("", s))

applyRules :: PhonemeClassMap -> String -> [Rule] -> String
applyRules classes = foldl (applyRule classes)

-- Context determination

matchContext :: PhonemeClassMap -> Context -> Context -> Bool
matchContext classes = (==)