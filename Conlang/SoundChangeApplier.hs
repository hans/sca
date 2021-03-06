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

parseContextElement :: PhonemeClassMap -> String -> Maybe (ContextElement, String)
parseContextElement m (c:s) | isPhonemeClass c
                              = Just ((PhonemeClassContext c (m ! c)), s)
                            | isPhoneme c
                              = Just ((PhonemeContext c), s)
                            | otherwise
                              = Nothing
parseContextElement _ ""      = Nothing

-- Accepts a previous context and a function for continuing with such a
-- after-context as contained within the context, and returns a new
-- context (context context context)
bindParser :: Maybe (ContextElement, String)
              -> (String -> Maybe (ContextElement, String))
              -> Maybe (ContextElement, String)

bindParser Nothing       _ = Nothing
bindParser (Just (_, s)) f = f s

-- Predicate: Is the Maybe a Just?
matchJust :: Maybe a -> Bool
matchJust (Just _) = True
matchJust Nothing  = False

pullContextElement :: Maybe (ContextElement, String) -> ContextElement
pullContextElement (Just (c, _)) = c

parseContext :: PhonemeClassMap -> String -> [ContextElement]
parseContext m s = map pullContextElement
                       (takeWhile matchJust (iterate (flip bindParser p)
                                                (p s)))
                   where p = parseContextElement m

-- Context-free rule match - this is the core logic!
matchContextElement :: ContextElement -> Char -> Bool
matchContextElement (PhonemeClassContext _ cs) d = d `elem` cs
matchContextElement (PhonemeContext c) d = c == d

matchContext :: Context -> String -> Bool
matchContext (c:cs) (s:ss) = matchContextElement c s
                          && matchContext cs ss
matchContext []     []     = True
matchContext _      _      = False

-- Rule application with context tracking

takeLast :: Int -> [a] -> [a]
takeLast n xs = drop (length xs - n) xs

type SampleContext = (String, String)

applyRule' :: SampleContext -> Rule -> Maybe (String, SampleContext)

applyRule' (b, (i:a)) r
    = if matchContext bc bt
         && matchContext ic [i]
         && matchContext ac at
      then Just (replacement r, newSampleContext)
      else Just ([i], newSampleContext)
      where bc = beforeContext r
            ic = inContext r
            ac = afterContext r

            -- Trim sample contexts to match length of rule's context
            bt = takeLast (length bc) b
            at = take (length ac) a

            newSampleContext = (b ++ [i], a)

applyRule' (_, []) r = Nothing

applyRule :: String -> String -> Rule -> String
applyRule b a r = case applyRule' (b, a) r of
                      Just (x, (y, z)) -> x ++ applyRule y z r
                      Nothing          -> []

applyRules :: String -> [Rule] -> String
applyRules = foldl (applyRule "")