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
phonemeClassDefinition = modifyState =<< modifier
                         where modifier = liftM2 insert upper defn
                               defn     = char ':' >> spaces >> many1 lower

context :: Parsec String PhonemeClassMap Context
context = many1 (classContext <|> phonemeContext)

classContext :: Parsec String PhonemeClassMap ContextElement
classContext = liftM oneOf $ liftM2 (!) getState upper

phonemeContext :: Parsec String PhonemeClassMap ContextElement
phonemeContext = liftM char lower

rule :: Parsec String PhonemeClassMap Rule
rule = do
    inContext <- spaces >> context
    replacement <- spaces >> char '>' >> spaces >> (many1 $ noneOf " ")
    beforeContext <- spaces >> char '/' >> spaces >> context
    afterContext <- char '_' >> context

    return $ Rule replacement beforeContext inContext afterContext

file :: Parsec String PhonemeClassMap [Rule]
file = many phonemeClassDefinition >> many rule

main :: IO ()
main = do
    case runParser file empty "" "V: aeiou\nr > l / V_V\ns > r / V_V" of
         (Right (r:rs)) -> print $ parse (head (beforeContext r)) "" "aro"
         _ -> print ":("
