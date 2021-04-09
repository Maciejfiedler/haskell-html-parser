# haskell-html-parser

Libraries required:
* http-conduit
* parsec

```haskell
import qualified Network.HTTP.Simple as Net
import qualified Data.ByteString as BS
import Data.Either
import Control.Monad

import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.ByteString

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

getLinksFromParsedHTML:: Either ParseError [HTMLExpr] -> [HTMLExpr]
getLinksFromParsedHTML list = either (\left -> []) getLinks list
  where getLinks = \a -> filter (\b -> b /= Other ()) a

main :: IO ()
main = do
    let url = "https://haskell.org"
    let request = Net.parseRequest_ url
    response <- Net.httpBS request
    let bsResponseBody = Net.getResponseBody response
    let parsedHTML = simpleParser parseHTML bsResponseBody
    print $ show $ getLinksFromParsedHTML parsedHTML

```
