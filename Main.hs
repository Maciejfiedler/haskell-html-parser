import Network.HTTP 

url = "http://www.happylearnhaskelltutorial.com/contents.html"

response = simpleHTTP (getRequest url)

pageContent = response >>= getResponseBody

-- Todo: check statusCode

main = do
  pageContentInString <- pageContent
  putStrLn pageContentInString
