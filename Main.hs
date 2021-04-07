import Network.HTTP.Simple
import qualified Data.ByteString as BS
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
    let insideA = between (lexeme $ string "<a>") (lexeme $ string "</a>")
    void $ insideA $ string "href=" -- go to href
    void $ char '"' -- open href
    link <- parseLink
    void $ char '"' -- close href
    return $ Link link
  where parseLink = many1 $ satisfy (\a -> a /= '"')

parseHTML :: Parser [HTMLExpr]
parseHTML = many parseHTMLLink

main :: IO ()
main = do
    r <- response
    let bsResponseBody = getResponseBody r
    let links = simpleParser parseHTML bsResponseBody
    print $ show links

