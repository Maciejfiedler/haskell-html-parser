import Network.HTTP.Simple

import Control.Monad

import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.ByteString
import Text.Parsec.Token

url = "https://haskell.org"
request = parseRequest_ url
response = httpBS request
responseInString = response >>= putStrLn . show

-- Expressions to parse
data HTMLExpr = Link String
                deriving (Eq, Show)

parseHTMLLink :: Parser HTMLExpr
parseHTMLLink = do
    between ("<a>" <$> symbol) ("</a>" <$> symbol) (string "test")
    return $ Link "test"
