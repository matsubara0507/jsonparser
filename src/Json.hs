module JSON
    ( JSON
    , Pair
    , JValue (..)
    , parseJson
    , jsonParser
    ) where

import           Data.Char            (chr, toUpper)
import           Data.Functor         (($>))
import           Data.Maybe           (fromJust)
import           Text.Megaparsec      (ParseError, Parsec, between, count, eof,
                                       many, runParser, sepBy, some, try, (<|>))
import           Text.Megaparsec.Char (char, digitChar, hexDigitChar, noneOf,
                                       oneOf, space, string)

type JSON = JValue

data JValue
  = JNull
  | JNumber Double
  | JString String
  | JBool   Bool
  | JObject [Pair]
  | JArray [JValue]
  deriving (Show, Eq)

type Pair = (String, JValue)

type Parser = Parsec String String

parseJson :: String -> Either (ParseError Char String) JSON
parseJson = runParser (jsonParser <* eof) ""

jsonParser :: Parser JSON
jsonParser = token valueParser

token :: Parser a -> Parser a
token p = space *> p <* space

symbol :: String -> Parser String
symbol = token . string

objectParser :: Parser [Pair]
objectParser = between (symbol "{") (symbol "}") membersParser

membersParser :: Parser [Pair]
membersParser = pairParser `sepBy` char ','

pairParser :: Parser (String, JValue)
pairParser = (,) <$> (token stringParser <* char ':') <*> token valueParser

valueParser :: Parser JValue
valueParser = JString <$> stringParser
          <|> JNumber <$> numberParser
          <|> JObject <$> objectParser
          <|> JArray  <$> arrayParser
          <|> JBool   <$> boolParser
          <|> const JNull <$> string "null"

stringParser :: Parser String
stringParser = between (string "\"") (string "\"") (many charParser)

charParser :: Parser Char
charParser = noneOf ['\"', '\\', '\t', '\n', '\NUL'] <|> (char '\\' *> charParser')
  where
    charParser' = oneOf ['\"', '\\', '/']
              <|> char 'b' $> '\b'
              <|> char 'f' $> '\f'
              <|> char 'n' $> '\n'
              <|> char 'r' $> '\r'
              <|> char 't' $> '\t'
              <|> char 'u' *> (chr . utf2int <$> count 4 hexDigitChar)

utf2int :: [Char] -> Int
utf2int = sum . zipWith (*) [4096,256,16,1] . fmap hex2int
  where
    hex2int = fromJust . flip lookup hexis . toUpper
    hexis = zip "0123456789ABCDEF" [0..]

numberParser :: Parser Double
numberParser = read <$> numberParser'
  where
    numberParser' =
      mappend <$> intParser <*> (try fracParser <|> try expParser <|> pure "")

intParser :: Parser String
intParser = ((:) <$> char '-' <*> intParser') <|> intParser'
  where
    intParser' = string "0"
             <|> (:) <$> oneOf ['1'..'9'] <*> digitsParser

digitsParser :: Parser String
digitsParser = many digitChar

digitsParser' :: Parser String
digitsParser' = some digitChar

fracParser :: Parser String
fracParser = mappend <$> ((:) <$> char '.' <*> digitsParser')
                     <*> (expParser <|> pure "")

expParser :: Parser String
expParser = mappend <$> eParser <*> digitsParser'

eParser :: Parser String
eParser = (:) <$> oneOf ['e', 'E'] <*> (string "+" <|> string "-" <|> pure "")

arrayParser :: Parser [JValue]
arrayParser = between (symbol "[") (symbol "]") elementsParser

elementsParser :: Parser [JValue]
elementsParser = token valueParser `sepBy` char ','

boolParser :: Parser Bool
boolParser = const True  <$> string "true"
         <|> const False <$> string "false"
