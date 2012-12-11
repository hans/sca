module SoundChangeParser where

import Control.Monad
import Text.Parsec

type Phoneme = Char

data ContextElement = PhonemeClassContext Char
                    | PhonemeContext Phoneme

type Context = [ContextElement]

data Rule = Rule { replacement   :: [Phoneme],
                   beforeContext :: Context,
                   inContext     :: Context,
                   afterContext  :: Context }

instance Show ContextElement where
    show (PhonemeClassContext c) = [c]
    show (PhonemeContext c) = [c]

instance Show Rule where
    show (Rule r b i a) = show i ++ " > " ++ r ++ " / " ++ show b ++ "_" ++ show a

context :: Parsec String () Context
context = many1 (classContext <|> phonemeContext)

classContext :: Parsec String () ContextElement
classContext = liftM PhonemeClassContext upper

phonemeContext :: Parsec String () ContextElement
phonemeContext = liftM PhonemeContext lower

rule :: Parsec String () Rule
rule = do
    spaces
    inContext <- context
    spaces >> char '>' >> spaces
    replacement <- many1 $ noneOf " "
    spaces >> char '/' >> spaces
    beforeContext <- context
    char '_'
    afterContext <- context

    return $ Rule replacement beforeContext inContext afterContext


main :: IO ()
main = do
       print $ parse rule "" "r > l / V_V"