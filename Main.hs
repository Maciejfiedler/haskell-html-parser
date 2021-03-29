import Network.HTTP.Simple
import qualified Data.ByteString.Char8 as BS

request = parseRequest_ "https://google.com"

response = httpBS request

main = response >>= BS.putStrLn . getResponseBody
