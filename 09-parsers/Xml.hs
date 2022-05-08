{-
---
fulltitle: "In class exercise: XML parsing"
---

In today's exercise you will use the definitions from the `Parsers` lecture to
build a simple parser for `XML` data.
-}

module Xml where

import Control.Applicative (Alternative (..))
{-
This exercise is based on the following definitions from the `Parsers`
lecture. Make sure that you have downloaded the solution.
-}

import ParserCombinators (Parser, char, doParse, filter, satisfy, string)
import System.IO
import Prelude hiding (filter)

-- Note that this line imports these functions as well as the instance for Parser
-- for the Functor, Applicative and Alternative classes.

{-
Your goal: produce this structured data from a string
-}

-- | A simplified datatype for storing XML
data SimpleXML
  = PCDATA String
  | Element ElementName [SimpleXML]
  deriving (Show)

type ElementName = String

{-
First: the characters `/`, `<`, and `>` are not allowed to appear in tags or
PCDATA. Let's define a function that recognizes them.
-}

reserved :: Char -> Bool
reserved c = c `elem` ['/', '<', '>']

{-
Use this definition to parse a maximal nonempty sequence of nonreserved characters:
-}

text :: Parser String
text = undefined

{-
~~~~{.haskell}
Xml> doParse text "skhdjf"
Just ("skhdjf","")
Xml> doParse text "akj<skdfsdhf"
Just ("akj","<skdfsdhf")
Xml> doParse text ""
Nothing
~~~~

and then use this definition to parse nonreserved characters into XML.
-}

pcdata :: Parser SimpleXML
pcdata = undefined

{-
~~~~{.haskell}
Xml> doParse pcdata "akj<skdfsdhf"
Just (PCDATA "akj","<skdfsdhf")
~~~~

Parse an empty element, like `"<br/>"`
-}

emptyContainer :: Parser SimpleXML
emptyContainer = undefined

{-
~~~~~{.haskell}
Xml> doParse emptyContainer "<br/>sdfsdf"
Just (Element "br" [],"sdfsdf")
~~~~~

Parse a container element: this consists of an open tag, a (potentially empty)
sequence of content parsed by `p`, and matching a closing tag.  For
example, `container pcdata` should recognize
`<br></br>` or `<title>A midsummer night's dream</title>`.  You do
NOT need to make sure that the closing tag matches the open tag.
-}

container :: Parser SimpleXML -> Parser SimpleXML
container p = undefined

{-
~~~~~{.haskell}
Xml> doParse (container pcdata) "<br></br>"
Just (Element "br" [],"")
Xml> doParse (container pcdata) "<title>A midsummer night's dream</title>"
Just (Element "title" [PCDATA "A midsummer night's dream"],"")

-- This should also work, even though the tag is wrong
Xml> doParse (container pcdata) "<title>A midsummer night's dream</br>"
Just (Element "title" [PCDATA "A midsummer night's dream"],"")
~~~~~

Now put the above together to construct a parser for simple XML data:
-}

xml :: Parser SimpleXML
xml = undefined

{-
~~~~~{.haskell}
Xml> doParse xml "<body>a</body>"
Just (Element "body" [PCDATA "a"],"")
Xml> doParse xml "<body><h1>A Midsummer Night's Dream</h1><h2>Dramatis Personae</h2>THESEUS, Duke of Athens.<br/>EGEUS, father to Hermia.<br/></body>"
Just (Element "body" [Element "h1" [PCDATA "A Midsummer Night's Dream"],Element "h2" [PCDATA "Dramatis Personae"],PCDATA "THESEUS, Duke of Athens.",Element "br" [],PCDATA "EGEUS, father to Hermia.",Element "br" []],"")
~~~~~

Now let's try it on something a little bigger. How about [`sample.html`](../../hw/hw02/sample.html) from hw02?
-}

-- | Run a parser on a particular input file
parseFromFile :: Parser a -> String -> IO (Maybe (a, String))
parseFromFile parser filename = do
  handle <- openFile filename ReadMode
  str <- hGetContents handle
  return $ doParse parser str

{-
~~~~~{.haskell}
Xml> parseFromFile xml "sample.html"
~~~~~

Challenge: rewrite container so that it only succeeds when the closing tag matches the opening
tag.
-}

container2 :: Parser SimpleXML -> Parser SimpleXML
container2 p = undefined

{-
~~~~~~{.haskell}
Xml> doParse (container2 pcdata) "<title>A midsummer night's dream</title>"
Just (Element "title" [PCDATA "A midsummer night's dream"],"")
Xml> doParse (container2 pcdata) "<title>A midsummer night's dream</br>"
Nothing
~~~~~~
-}
