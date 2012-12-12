module SoundChangeParser where

import Control.Monad
import Data.Map ((!), empty, insert, Map)
import Text.Parsec
import Text.Parsec.String (Parser)

type Phoneme = Char
type PhonemeClassMap = Map Char [Phoneme]

type ContextElement = Parser Phoneme
type Context = [ContextElement]

data Rule = Rule { replacement   :: [Phoneme],
                   beforeContext :: Context,
                   inContext     :: Context,
                   afterContext  :: Context }

-- TODO
instance Show Rule where
    show (Rule r b i a) = show r

phonemeClassDefinition :: Parsec String PhonemeClassMap ()
phonemeClassDefinition = do
    c <- upper
    string ":" >> spaces
    ps <- many1 lower
    modifyState (insert c ps)

context :: Parsec String PhonemeClassMap Context
context = many1 (classContext <|> phonemeContext)

classContext :: Parsec String PhonemeClassMap ContextElement
classContext = do
    c <- upper
    m <- getState
    return $ oneOf (m ! c)

phonemeContext :: Parsec String PhonemeClassMap ContextElement
phonemeContext = liftM char lower

rule :: Parsec String PhonemeClassMap Rule
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

file :: Parsec String PhonemeClassMap [Rule]
file = many phonemeClassDefinition >> many rule

main :: IO ()
main = do
    print $ runParser file empty "" "V: aeiou\nr > l / V_V\ns > r / V_V"