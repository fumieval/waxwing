{-# LANGUAGE  ApplicativeDo #-}
module Main where
import Control.Monad
import Options.Applicative as O

import Eval (execEval, evalBlock)
import Parser (parseFile)

main :: IO ()
main = join $ execParser (info app mempty)
  
app :: O.Parser (IO ())
app = do
  enableTrace <- switch $ long "trace"
  srcPath <- strArgument (metavar "SRC")
  pure $ do
    stmts <- parseFile srcPath
    execEval enableTrace $ evalBlock stmts
