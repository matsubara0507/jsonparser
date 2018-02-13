module JSON
    ( parseJson
    , jsonParser
    ) where

import           Data.Char            (chr, toUpper)
import           Data.Functor         (($>))
import           Data.Maybe           (fromJust)
import           Text.Megaparsec      (ParseError, Parsec, between, count, many,
                                       parse, sepBy, (<|>))
import           Text.Megaparsec.Char (char, digitChar, hexDigitChar, noneOf,
                                       oneOf, space, string)

type JSON = [(String, JValue)]

data JValue = JNull
            | JNumber Double
            | JString String
            | JBool   Bool
            | JArray  [JValue]
            | JObject JSON
            deriving (Show, Eq)

type Parser = Parsec String String

parseJson :: String -> Either (ParseError Char String) JSON
parseJson = parse jsonParser ""

jsonParser :: Parser JSON
jsonParser = objectParser

token :: Parser a -> Parser a
token p = space *> p <* space

symbol :: String -> Parser String
symbol = token . string

objectParser :: Parser JSON
objectParser = between (symbol "{") (symbol "}") membersParser

membersParser :: Parser JSON
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
charParser = noneOf ['\"', '\\'] <|> (char '\\' *> charParser')
  where
    charParser' = oneOf ['\"', '\\', '/']
              <|> char 'b' $> '\b'
              <|> char 'f' $> '\f'
              <|> char 'n' $> '\n'
              <|> char 'r' $> '\r'
              <|> char 't' $> '\t'
              <|> char 'u' *> (chr . sum . map hex2int <$> count 4 hexDigitChar)

hex2int :: Char -> Int
hex2int = fromJust . flip lookup hexis . toUpper
  where
    hexis = zip "0123456789ABCDEF" [0..]

numberParser :: Parser Double
numberParser = read <$> numberParser'
  where
    numberParser' = mappend <$> intParser
                <*> (fracParser <|> expParser <|> pure "")

intParser :: Parser String
intParser = (:) <$> char '-' <*> intParser' <|> intParser'
  where
    intParser' = string "0"
             <|> (:) <$> oneOf ['1'..'9'] <*> digitsParser

digitsParser :: Parser String
digitsParser = many digitChar

fracParser :: Parser String
fracParser = mappend <$> ((:) <$> char '.' <*> digitsParser)
                     <*> (expParser <|> pure "")

expParser :: Parser String
expParser = mappend <$> eParser <*> digitsParser

eParser :: Parser String
eParser = (:) <$> oneOf ['e', 'E'] <*> (string "+" <|> string "-" <|> pure "")

arrayParser :: Parser [JValue]
arrayParser = between (symbol "[") (symbol "]") elementsParser

elementsParser :: Parser [JValue]
elementsParser = token valueParser `sepBy` char ','

boolParser :: Parser Bool
boolParser = const True  <$> string "true"
         <|> const False <$> string "false"
