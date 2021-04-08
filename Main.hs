import Network.HTTP.Simple
import qualified Data.ByteString as BS
import Data.Either
import Control.Monad

import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.ByteString

url = "https://haskell.org"
request = parseRequest_ url
response = httpBS request
responseInString = response >>= putStrLn . show

-- Expressions to parse
data HTMLExpr = Link String
              | Other ()
                deriving (Eq, Show)

whitespace :: Parser ()
whitespace = void $ many $ oneOf " \n\t"

lexeme :: Parser a -> Parser a
lexeme p = do
           x <- p
           whitespace
           return x

simpleParser :: Parser a -> BS.ByteString -> Either ParseError a
simpleParser p = parse p ""

parseHTMLLink :: Parser HTMLExpr
parseHTMLLink = do
    void $ lexeme $ string "<a" -- go to a element
    void $ manyTill anyChar (try (string "href="))
    void $ char '"' -- open href
    link <- parseLink
    void $ char '"' -- close href
    void $ manyTill anyChar (try (string ">")) -- close element
    return $ Link link
  where parseLink = many1 $ satisfy (\a -> a /= '"')

parseHTMLOther :: Parser HTMLExpr
parseHTMLOther = do
    void $ anyChar
    return $ Other ()

parseHTML :: Parser [HTMLExpr]
parseHTML = many $ try parseHTMLLink <|> parseHTMLOther

getLinksFromParsedList :: Either ParseError [HTMLExpr] -> [HTMLExpr]
getLinksFromParsedList list = do
    either (\left -> []) getLinks list
  where getLinks = \a -> filter (\b -> b /= Other ()) a

main :: IO ()
main = do
    r <- response
    let bsResponseBody = getResponseBody r
    let parsedHTML = simpleParser parseHTML bsResponseBody
    print $ show $ getLinksFromParsedList parsedHTML

