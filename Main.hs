import Network.HTTP.Simple

request = parseRequest_ "https://google.com"

response = httpBS request

main = response >>= putStrLn . show
