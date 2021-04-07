import Network.HTTP.Simple
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.ByteString
import qualified Text.Parsec.Token as T

url = "https://haskell.org"
request = parseRequest_ url
response = httpBS request
responseInString = response >>= putStrLn . show

-- Expressions to parse
data HTMLExpr = Link String
                deriving (Eq, Show)

parseHTMLLink :: Parser HTMLExpr
parseHTMLLink = do
    skipMany1 notLink
    insideA = between (T.symbol "<a>") (T.symbol "</a>") 
    -- go to href
    void $ insideA $ T.lexeme $ string "href=\""
    -- get link
    parseLink <- link
    -- close href
    void $ char '"'


  where 
    notLink = satisfy (\a -> /= "<a>")
    link = satisfy (\a -> /= '"')
