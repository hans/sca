module Conlang.SoundChangeApplier where

import Data.Char (isLetter, isUpper)
import Data.Map ((!), fromList, Map)

-- A single IPA char
--
-- TODO: Phonemes can have more than one character (palatalized,
-- ejective, etc. modifiers)
type Phoneme = Char

type PhonemeClassMap = Map Char [Phoneme]

data ContextElement = PhonemeClassContext Char String
                    | PhonemeContext Phoneme

type Context = [ContextElement]

-- Rule for matching a phoneme or phoneme class within a context
data Rule = Rule { replacement   :: [Phoneme],
                   beforeContext :: Context,
                   inContext     :: Context,
                   afterContext  :: Context }

instance Show ContextElement where
    show (PhonemeClassContext c _) = [c]
    show (PhonemeContext c) = [c]

instance Show Rule where
    show (Rule r b i a) = show i ++ " > " ++ r ++ " / " ++ show b ++ "_" ++ show a

-- Parse context

isPhonemeClass :: Char -> Bool
isPhonemeClass = isUpper

isPhoneme :: Char -> Bool
isPhoneme = isLetter

parseContext :: PhonemeClassMap -> String -> Maybe (ContextElement, String)
parseContext m (c:s) | isPhonemeClass c
                       = Just ((PhonemeClassContext c (m ! c)), s)
                     | isPhoneme c
                       = Just ((PhonemeContext c), s)
                     | otherwise
                       = Nothing

-- Context-free rule match - this is the core logic!
matchContextElement :: ContextElement -> Char -> Bool
matchContextElement (PhonemeClassContext _ cs) d = d `elem` cs
matchContextElement (PhonemeContext c) d = c == d

matchContext :: Context -> String -> Bool
matchContext (c:cs) (s:ss) = matchContextElement c s
                          && matchContext cs ss
matchContext []     []     = True
matchContext _      _      = False

-- -- Rule application with context tracking

-- takeLast :: Int -> [a] -> [a]
-- takeLast n xs = drop (length xs - n) xs

-- -- Context-free rule match. Innermost and simplest logic goes here.
-- matchRule :: PhonemeClassMap -> Rule -> Char -> Bool

-- -- A phoneme class rule should match when the given char is a phoneme
-- -- class label.
-- matchRule classes (PhonemeClassRule l _ _ _) c = c `elem` (classes ! l)

-- -- A phoneme rule should match when the given char is equal to the
-- -- rule's match char.
-- matchRule _ (PhonemeRule l _ _ _) c = c == l

-- -- Context-aware rule application. Delegates primary rule logic
-- -- (context-free) to `matchRule` and context matching to `matchContext`.
-- applyRule' :: PhonemeClassMap -> Rule -> (Context, String) -> (Context, String)

-- applyRule' cs r (preceding, c : s)
--            = if    matchRule cs r c
--                 && matchContext cs (beforeContext r) bc
--                 && matchContext cs (afterContext r) ac
--              then (preceding ++ replacement r, s)
--              else (preceding ++ [c], s)
--              where
--                 -- Trim before-contexts and after-contexts to match what
--                 -- is needed by the rule
--                 bc = takeLast (length (beforeContext r)) preceding
--                 ac = take (length (afterContext r)) s

-- applyRule' _  _ (x, "") = (x, "")

-- applyRule :: PhonemeClassMap -> String -> Rule -> String
-- applyRule classes s r = fst . head $ dropWhile (\x -> snd x /= "")
--                                      (iterate (applyRule' classes r) ("", s))

-- applyRules :: PhonemeClassMap -> String -> [Rule] -> String
-- applyRules classes = foldl (applyRule classes)

-- -- Context determination



-- isPhonemeClass :: Char -> Bool
-- isPhonemeClass = isUpper

-- matchContext :: PhonemeClassMap -> Context -> String -> Bool
-- matchContext classes (c:cs) (d:ds)
--              | isPhonemeClass c  = member c classes
--                                    && d `elem` (classes ! c)
--                                    && matchContext classes cs ds
--              | otherwise         = c == d
--                                    && matchContext classes cs ds
-- matchContext _       []     [] = True
-- matchContext _       []     _  = False
-- matchContext _       _      [] = False