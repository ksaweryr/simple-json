{-# Language LambdaCase #-}
module Lib (JsonValue, parseJson, dumpJson) where

import Control.Applicative
import Control.Monad (liftM2)
import Data.Bifunctor (first)
import qualified Data.Map.Strict as M

data JsonValue = JsonObject (M.Map String JsonValue)
               | JsonArray [JsonValue]
               | JsonString String
               | JsonNumber Double
               | JsonBool Bool
               | JsonNull
               deriving (Show)

parseJson :: String -> Maybe JsonValue
parseJson = (fmap fst) . runParser json

dumpJson :: JsonValue -> String
dumpJson (JsonObject m) = '{' : (foldl1 (\b a -> b ++ ", " ++ a) (map (\(k, v) -> show k ++ ": " ++ dumpJson v) (M.toList m))) ++ "}"
dumpJson (JsonArray a)  = '[' : (foldl1 (\b a -> b ++ ", " ++ a) (map dumpJson a)) ++ "]"
dumpJson (JsonString s) = show s
dumpJson (JsonNumber n) = show n
dumpJson (JsonBool b)   = show b
dumpJson JsonNull       = "null"

-- TODO: error support
newtype Parser a =  Parser { runParser :: String -> Maybe (a, String) }

instance Functor Parser where
  fmap f (Parser p) = Parser $ fmap (first f) . p

instance Applicative Parser where
  pure a = Parser $ \s -> Just (a, s)
  (Parser p) <*> (Parser q) = Parser $ \s -> do
    (f, s') <- p s
    (a, s'') <- q s'
    return (f a, s'')

instance Alternative Parser where
  empty = Parser $ const Nothing
  (Parser p) <|> (Parser q) = Parser $ liftM2 (<|>) p q

instance Semigroup a => Semigroup (Parser a) where
  (Parser p) <> (Parser q) = Parser $ \s -> do
    (a, s') <- p s
    (b, s'') <- q s'
    return (a <> b, s'')

predP :: (Char -> Bool) -> Parser Char
predP pred = Parser $ \case
  (x:xs) -> if pred x then Just (x, xs) else Nothing
  ""     -> Nothing

charP :: Char -> Parser Char
charP c = predP (== c)

stringP :: String -> Parser String
stringP = sequenceA . map charP

sepByP :: Parser String -> Parser a -> Parser [a]
sepByP separator p = (:) <$> p <*> many (separator *> p)

json :: Parser JsonValue
json = element

value :: Parser JsonValue
value = Lib.bool <|> Lib.null <|> ((JsonNumber . read) <$> number) <|> (JsonString <$> string) <|> array <|> object

bool :: Parser JsonValue
bool = (JsonBool . toBool) <$> (stringP "true" <|> stringP "false")
  where
    toBool "true"  = True
    toBool "false" = False
    toBool _       = error "Should never happen (parse bool)"

null :: Parser JsonValue
null = stringP "null" *> pure JsonNull

object :: Parser JsonValue
object = (JsonObject . M.fromList) <$> (charP '{' *> ((sepByP (stringP ", ") member) <|> (ws *> pure [])) <* charP '}')
  where
    member :: Parser (String, JsonValue)
    member = (,) <$> (ws *> string <* ws <* charP ':') <*> element

array :: Parser JsonValue
array = JsonArray <$> (charP '[' *> ((sepByP (stringP ",") element) <|> (ws *> pure [])) <* charP ']')

element :: Parser JsonValue
element = ws *> value <* ws

string :: Parser String
string = charP '"' *> ((foldl (<>) "") <$> (many character)) <* charP '"'

character :: Parser String
character = (stringP "\\" *> escape) <|> (pure <$> predP (liftM2 (&&) (/= '"') (/= '\\')))

escape :: Parser String
escape = (foldl1 (<|>) $ map (stringP . pure) "\"\\/bfnrt") <|> ((:) <$> charP 'u' <*> sequenceA (replicate 4 hex))

hex :: Parser Char
hex = foldl1 (<|>) (map charP $ ['0'..'9'] <> ['A'..'F'] <> ['a'..'f'])

number :: Parser String
number = integer <> fraction <> Lib.exponent

integer :: Parser String
integer = (stringP "-" <|> pure "") <> (((:) <$> onenine <*> some digit) <|> (pure <$> digit))

digit :: Parser Char
digit = charP '0' <|> onenine

onenine :: Parser Char
onenine = foldl1 (<|>) (map charP ['1'..'9'])

fraction :: Parser String
fraction = ((:) <$> charP '.' <*> some digit) <|> pure ""

exponent :: Parser String
exponent = ((:) <$> (charP 'E' <|> charP 'e') <*> sign <> some digit) <|> pure ""

sign :: Parser String
sign = stringP "+" <|> stringP "-" <|> pure ""

ws :: Parser String
ws = many $ foldl1 (<|>) (map charP " \n\r\t")