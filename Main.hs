module Main where

import           Data.Void (Void)
import           Text.Megaparsec as P (Parsec, try, many, parse, errorBundlePretty, choice, satisfy, between, MonadParsec (eof))
import           Text.Megaparsec.Char (string, space1, letterChar, char, digitChar)
import qualified Text.Megaparsec.Char.Lexer as L
import           System.Environment (getArgs)
import           System.FilePath.Posix ((<.>), dropExtension, (</>), splitFileName)
import Data.Char ( isPrint )
import System.Directory (createDirectoryIfMissing)

type Parser = Parsec Void String

whitespace :: Parser ()
whitespace = L.space
  space1
  (L.skipLineComment "//")
  (L.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme whitespace

reserved :: String -> Parser String
reserved = lexeme . P.try . string

integer :: Parser Integer
integer =  lexeme L.decimal

symbol :: String -> Parser String
symbol = L.symbol whitespace

quotes :: Parser a -> Parser a
quotes = between (symbol "\"") (symbol "\"")

xmlWrap :: String -> String -> String
xmlWrap t x = "<" ++ t ++ "> " ++ x ++ " </" ++ t ++ ">"

tKeyword :: Parser String
tKeyword = choice $ map ((xmlWrap "keyword" <$>) . reserved)
  [ "class"
  , "constructor"
  , "function"
  , "method"
  , "field"
  , "static"
  , "var"
  , "int"
  , "char"
  , "boolean"
  , "void"
  , "true"
  , "false"
  , "null"
  , "this"
  , "let"
  , "do"
  , "if"
  , "else"
  , "while"
  , "return" ]

pSym :: String -> String
pSym "<"  = "&lt;"
pSym ">"  = "&gt;"
pSym "\"" = "&quot;"
pSym "&"  = "&amp;"
pSym s    = s

tSymbol :: Parser String
tSymbol = choice $ map ((xmlWrap "symbol" . pSym <$>) . reserved)
  [ "{"
  , "}"
  , "("
  , ")"
  , "["
  , "]"
  , "."
  , ","
  , ";"
  , "+"
  , "-"
  , "*"
  , "/"
  , "&"
  , "|"
  , "<"
  , ">"
  , "="
  , "~" ]

tInteger :: Parser String
tInteger = xmlWrap "integerConstant" . show <$> integer

isValid :: Char -> Bool
isValid '\n' = False
isValid '"'  = False
isValid c    = isPrint c

stringChar :: Parser Char
stringChar = satisfy isValid

tStr :: Parser String
tStr = quotes $ many stringChar

tString :: Parser String
tString = xmlWrap "stringConstant" <$> tStr

tIdentifier :: Parser String
tIdentifier = do
  a <- choice
    [ letterChar
    , char '_' ]
  rest <-  many $ choice
    [ letterChar
    , digitChar
    , char '_' ]
  return $ xmlWrap "identifier" $ a:rest

tState :: Parser String
tState = whitespace *> choice
  [ try tKeyword
  , try tSymbol
  , try tInteger
  , try tString
  , tIdentifier ]

tJack :: Parser [String]
tJack = whitespace *> many tState <* eof

tokenize :: String -> String
tokenize s = case parse tJack "" s of
  Left err -> error (errorBundlePretty err)
  Right t -> "<tokens>\n" ++ unlines t ++ "</tokens>\n"

process :: String -> IO String
process filepath = do
  contents <- readFile filepath
  return $ tokenize contents

changeName :: String -> IO String
changeName s = do
  let (dir, file) = splitFileName s
  let base = dropExtension file
  createDirectoryIfMissing False (dir ++ "T")
  return $ (dir ++ "T") </> (base ++ "T") <.> "xml"

main :: IO ()
main = do
  args <- getArgs
  results <- process $ head args
  name <- changeName $ head args
  writeFile name results
  putStrLn $ "written file to " ++ name