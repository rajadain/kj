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

data ScriptData = ScriptData { _scriptData_name :: String
                             , _scriptData_comment :: String
                             , _scriptData_args :: [String] -- TODO: currently unimplemented
                             }

instance Show ScriptData where
  show sd = printf "%-25s %s" (_scriptData_name sd) (_scriptData_comment sd)

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
emptyLine :: Parser ParseLine
emptyLine = endOfLine >> return Useless

whitespaceLine :: Parser ParseLine
whitespaceLine = trailingSpaceEol >> return Useless

sheBangLine :: Parser ParseLine
sheBangLine = string "#!" >> nonEolChars >> endOfLine >> return Useless

sourceLine :: Parser ParseLine
sourceLine = noneOf "#" >> nonEolChars >> endOfLine >> return Useless

-- mid-line parsers
trailingSpaceEol :: Parser Char
trailingSpaceEol = nonEolSpaces >> endOfLine

nonEolSpaces :: Parser String
nonEolSpaces = many (oneOf " \t\f\v\xa0")

nonEolChars :: Parser String
nonEolChars = many (noneOf "\n\r")
