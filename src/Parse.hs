module Parse (parseScript) where

import Text.Parsec hiding (Line, sourceLine)
import Text.Parsec.String
import Text.Printf

------------------------------------------------------------
-- public api
------------------------------------------------------------

parseScript :: String -> String -> Maybe ScriptData
parseScript fname script =
  let parsed = parse (text fname) "(script parse)" script
  in case parsed of (Left _) -> Nothing
                    (Right x) -> Just x

------------------------------------------------------------
-- types
------------------------------------------------------------

data ScriptData = ScriptData { name :: String, comment :: String,
                               args :: [String] }

instance Show ScriptData where
  show sd = printf "%-25s %s" (name sd) (comment sd)

data ParseLine = Useless | Args [String] | Comment String


isUseful :: ParseLine -> Bool
isUseful Useless = False
isUseful _ = True

isComment :: ParseLine -> Bool
isComment (Comment _) = True
isComment _ = False

------------------------------------------------------------
-- internal parsers
------------------------------------------------------------

text :: String -> Parser ScriptData
text filename = do
  xs <- many line
  -- make a singleton list with the first comment, which is all we'll use for now
  let usefuls = filter isUseful xs
  let comments = take 1 . filter isComment $ usefuls
  let args = take 1 . filter (not . isComment) $ usefuls
  let comment = case comments of (Comment x:_) -> x
                                 _ -> ""
  let args' = case args of Args x:_ -> x
                           _ -> []
  return (ScriptData filename comment args')
  
line :: Parser ParseLine
line =
  try argLine <|>
  try commentLine <|>
  try sheBangLine <|>
  try whitespaceLine <|>
  try emptyLine <|>
  sourceLine <?> "didn't find commenty or whitespace"

-- parsers that gather data we care about
argLine :: Parser ParseLine
argLine = do
  _ <- string "# args: "
  open <- char '['
  inner <- many (noneOf "[]")
  close <- char ']'
  _ <- trailingSpaceEol
  return $ Args $ read $ [open] ++ inner ++ [close]

commentLine :: Parser ParseLine
commentLine = do
  _ <- char '#'
  _ <- optional $ char ' '
  x <- noneOf "!"
  xs <- nonEolChars
  _ <- trailingSpaceEol
  return $ Comment (x:xs)

-- line parsers that return discardable results
emptyLine = endOfLine >> return Useless
whitespaceLine = trailingSpaceEol >> return Useless
sheBangLine = string "#!" >> nonEolChars >> endOfLine >> return Useless
sourceLine = noneOf "#" >> nonEolChars >> endOfLine >> return Useless

-- mid-line parsers
trailingSpaceEol = nonEolSpaces >> endOfLine
nonEolSpaces = many (oneOf " \t\f\v\xa0")
nonEolChars = many (noneOf "\n\r")
