module JSON
    ( jsonParser
    ) where

import Data.Char (chr, toUpper)
import Data.Maybe (fromJust)
import Data.Text (Text, pack)
import Text.Megaparsec ( between, count, some, many, oneOf, noneOf, (<|>), sepBy
                       , space, string, char, digitChar, hexDigitChar)
import Text.Megaparsec.String (Parser)

type JSON = [(String, JValue)]
data JValue = JNull
            | JNumber  Double
            | JString String
            | JBool   Bool
            | JArray  [JValue]
            | JObject JSON
            deriving (Show, Eq)

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
              <|> char 'b' *> pure '\b'
              <|> char 'f' *> pure '\f'
              <|> char 'n' *> pure '\n'
              <|> char 'r' *> pure '\r'
              <|> char 't' *> pure '\t'
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
intParser = ((:) <$> char '-' <*> intParser' <|> intParser')
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
