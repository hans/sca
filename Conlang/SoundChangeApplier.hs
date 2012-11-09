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

-- Rule application

-- applyRule classes (c:cs) r@(PhonemeRule l ps _ _)
--           | c == l     = ps ++ applyRule classes cs r
--           | otherwise  = c   : applyRule classes cs r

-- applyRule _ "" _ = ""

-- Rule application with context tracking

takeLast :: Int -> [a] -> [a]
takeLast n xs = drop ((length xs) - n) xs

applyRule' :: PhonemeClassMap -> Rule -> (Context, String) -> (Context, String)
applyRule' classes r@(PhonemeClassRule l ps bc ac) (bc', s@(c:cs))
           | length bc' < length bc || length cs < length ac
             = (bc' ++ [c], cs)
           | otherwise =
             let
                 -- Trim before-contexts and after-contexts to match what
                 -- is needed by the rule
                 ac'' = take (length ac) cs
                 bc'' = takeLast (length bc) bc'
             in
                 if    c `elem` (classes ! l)
                    && matchContext ac ac''
                    && matchContext bc bc''
                 then (bc' ++ ps , cs)
                 else (bc' ++ [c], cs)

applyRule' _ r@(PhonemeRule l r bc ac) (bc', s@(c:cs))
           | c == l    = (bc' ++ ps, cs)
           | otherwise = (bc' ++ [c], cs)

applyRule' _ _ (x, "") = (x, "")

applyRule :: PhonemeClassMap -> String -> Rule -> String
applyRule classes s r = fst . head $ dropWhile (\x -> snd x /= "")
                                     (iterate (applyRule' classes r) ("", s))

applyRules :: PhonemeClassMap -> String -> [Rule] -> String
applyRules classes = foldl (applyRule classes)

-- Context determination

matchContext :: Context -> Context -> Bool
matchContext = (==)