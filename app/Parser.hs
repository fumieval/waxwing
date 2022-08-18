module Parser where
import Control.Monad
import Data.List.NonEmpty (NonEmpty(..), some1)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L
import Control.Monad.Combinators.Expr
import AST
import Data.Void

type Parser = Parsec Void String

parseFile :: FilePath -> IO (NonEmpty (Stmt Parsed))
parseFile srcPath = do
  src <- readFile srcPath
  case runParser (stmts <* eof) srcPath src of
    Left e -> fail $ errorBundlePretty e
    Right ast -> pure ast

stmts :: Parser (NonEmpty (Stmt Parsed))
stmts = some1 stmt

block :: Parser (NonEmpty (Stmt Parsed))
block = between (symbol "{") (symbol "}") stmts

stmt :: Parser (Stmt Parsed)
stmt = choice
  [ do
    symbol "arg"
    ps <- some1 pat
    symbol ";"
    pure $ ArgS ps
  , try $ do
    p <- pat
    symbol "="
    e <- expr
    symbol ";"
    pure $ BindS p e
  , ReturnS <$> expr
  ] <?> "stmt"

expr :: Parser (Expr Parsed)
expr = makeExprParser application
  [[ InfixL (binary "%") ]]
  <?> "expr"

pat :: Parser (Pat Parsed)
pat = lexeme "pat" $ choice
  [ between (symbol "(") (symbol ")") pat
  , do
    symbol "for"
    ForP <$> pat
  , VarP <$> identifier
  , LitP <$> literal
  ]

binary :: String -> Parser (Expr Parsed -> Expr Parsed -> Expr Parsed)
binary op = do
  symbol op
  pure $ \a b -> AppE (VarE op) (a :| [b])

application :: Parser (Expr Parsed)
application = go <$> term <*> many term where
  go t [] = t
  go f (t : ts) = AppE f (t :| ts)

term :: Parser (Expr Parsed)
term = choice
  [ do
    symbol "if"
    c <- block
    cs <- many $ symbol "else" *> block
    pure $ IfE (c :| cs)
  , between (symbol "(") (symbol ")") expr
  , DoE <$> block
  , LitE <$> literal
  , VarE <$> identifier
  ] <?> "term"

literal :: Parser Lit
literal = lexeme "literal" $ choice
  [ IntL <$> L.decimal
  , StrL <$> (char '"' >> manyTill L.charLiteral (char '"'))
  ]

lexeme :: String -> Parser a -> Parser a
lexeme name p = L.lexeme spaceConsumer (p <?> name)

symbol :: String -> Parser ()
symbol = void . L.symbol spaceConsumer

lineComment :: Parser ()
lineComment = L.skipLineComment "#"

spaceConsumer :: Parser ()
spaceConsumer = L.space
  space1
  lineComment
  empty

identifier :: Parser (Ident Parsed)
identifier = lexeme "identifier"
  $ (:) <$> letterChar <*> many alphaNumChar
