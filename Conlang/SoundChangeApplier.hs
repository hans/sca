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
takeLast n xs = drop ((length xs) - n) xs


matchRule :: PhonemeClassMap -> Rule -> Context -> Context -> Char -> Bool

matchRule classes (PhonemeClassRule l ps bc ac) bc' ac' c
          | length bc' < length bc || length ac' < length ac
            = False
          | otherwise = c `elem` (classes ! l)
                        -- && matchContext ac ac'
                        -- && matchContext bc bc'

matchRule _ (PhonemeRule l r bc ac) bc' ac' c
          | length bc' < length bc || length ac' < length ac
            = False
          | otherwise = c == l
                        && matchContext ac ac'
                        && matchContext bc bc'

applyRule' :: PhonemeClassMap -> Rule -> (Context, String) -> (Context, String)
applyRule' classes r (preceding, (c:cs))
           = if matchRule classes r bc ac c
             then (preceding ++ (replacement r), cs)
             else (preceding ++ [c], cs)
             where
                -- Trim before-contexts and after-contexts to match what
                -- is needed by the rule
                bc = takeLast (length (beforeContext r)) preceding
                ac = take (length (afterContext r)) cs

applyRule' _ _ (x, "") = (x, "")

applyRule :: PhonemeClassMap -> String -> Rule -> String
applyRule classes s r = fst . head $ dropWhile (\x -> snd x /= "")
                                     (iterate (applyRule' classes r) ("", s))

applyRules :: PhonemeClassMap -> String -> [Rule] -> String
applyRules classes = foldl (applyRule classes)

-- Context determination

matchContext :: Context -> Context -> Bool
matchContext = (==)