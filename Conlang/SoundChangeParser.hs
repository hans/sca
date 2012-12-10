module SoundChangeParser where

import Control.Monad
import Text.ParserCombinators.Parsec

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

context :: CharParser () Context
context = many1 (classContext <|> phonemeContext)

classContext :: CharParser () ContextElement
classContext = liftM PhonemeClassContext upper

phonemeContext :: CharParser () ContextElement
phonemeContext = liftM PhonemeContext lower

rule :: CharParser () Rule
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