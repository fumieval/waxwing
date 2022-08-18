{-# LANGUAGE OverloadedLists #-}
module Eval where

import AST
import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Data.Foldable
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import System.Exit

data Prim = ModP
  | ToStringP
  | RangeP
  | PrintP
  deriving Show

newtype Fun = Fun (NE.NonEmpty Value -> Eval Value)
instance Show Fun where
  show _ = "<function>"

data Value = IntV Integer
  | StrV String
  | NullV
  | FunV Fun
  | PrimV Prim
  | ListV [Value]
  deriving Show

data Env = Env
  { bound :: Map.Map String Value
  , enableTrace :: Bool
  }

askEnv :: Eval Env
askEnv = Eval $ \env cont _ -> cont env

instance MonadIO Eval where
  liftIO m = Eval $ \_ cont _ -> m >>= cont

newtype Eval a = Eval
  { unEval :: Env
    -> (a -> IO ())
    -> IO ()
    -> IO ()
  } deriving Functor

execEval :: Bool -> Eval Value -> IO ()
execEval enableTrace (Eval m) = m
  Env{..}
  (const mempty)
  exitFailure
  where
    (.=) = (,)
    bound = Map.fromList
      [ "%" .= PrimV ModP
      , "toString" .= PrimV ToStringP
      , "range" .= PrimV RangeP
      , "print" .= PrimV PrintP
      ]

instance Applicative Eval where
  pure x = Eval $ \_ cont _ -> cont x
  (<*>) = ap

instance Monad Eval where
  return = pure
  Eval m >>= k = Eval $ \env cont other -> m env (\a -> unEval (k a) env cont other) other

instance Alternative Eval where
  empty = Eval $ \_ _ other -> other
  Eval m <|> Eval n = Eval $ \env cont other -> m env cont (n env cont other)

apply :: Value -> NE.NonEmpty Value -> Eval Value
apply = \case
  PrimV p -> \xs -> case (p, xs) of
    (ModP, [IntV i, IntV j]) -> pure $ IntV (mod i j)
    (ToStringP, [IntV i]) -> pure $ StrV $ show i
    (RangeP, [IntV i, IntV j]) -> pure $ ListV $ map IntV [i..j]
    (PrintP, [StrV s]) -> NullV <$ liftIO (putStrLn s)
    _ -> error $ "Wrong type/number of arguments: " <> unwords (show p : map show (toList xs))
  FunV (Fun f) -> f
  v -> \xs -> error $ "Cannot apply " <> show v <> " to " <> unwords (map show $ toList xs)

traced :: (Show a, Show b) => (a -> Eval b) -> a -> Eval b
traced f a = do
  Env{..} <- askEnv
  if enableTrace
    then do
      liftIO $ putStrLn $ show a
      b <- f a
      liftIO $ putStrLn $ " ==> " <> show b
      pure b
    else f a

eval :: Expr Parsed -> Eval Value
eval = \case
  VarE k -> do
    Env{..} <- askEnv
    case Map.lookup k bound of
      Nothing -> error $ "unbound variable: " <> k
      Just v -> pure v
  AppE fun args -> do
    funV <- eval fun
    argsV <- traverse eval args
    apply funV argsV
  LitE lit -> pure $ case lit of
    IntL x -> IntV x
    StrL x -> StrV x
  DoE stmts -> evalBlock stmts
  IfE xss -> asum (fmap evalBlock xss)

evalBlock :: Stmts Parsed -> Eval Value
evalBlock stmts = case NE.last stmts of
  ReturnS expr -> evalStmts (eval expr) (NE.init stmts)
  _ -> error "the last statement of a block must be an expression"

evalStmts :: Eval Value -> [Stmt Parsed] -> Eval Value
evalStmts cont [] = cont
evalStmts cont (x : xs) = evalStmt (evalStmts cont xs) x

evalStmt :: Eval Value -> Stmt Parsed -> Eval Value
evalStmt cont stmt = case stmt of
  BindS pat expr -> do
    val <- traced eval expr
    match cont pat val
  ReturnS expr -> eval expr >> cont
  ArgS pats -> pure $ FunV $ Fun $ \vals -> if length pats == length vals
    then foldr
      (\(pat, val) r -> match r pat val)
      cont
      (NE.zip pats vals)
    else error "different number of arguments"
type Binding = (String, Value)

match :: Eval Value -> Pat Parsed -> Value -> Eval Value
match inner = \cases
  AnyP _ -> inner
  (ForP pat) (ListV xs) -> Eval $ \env cont other -> forM_ xs $ \x -> unEval (match inner pat x) env cont other
  (VarP name) val -> Eval $ \env cont other -> unEval inner env { bound = Map.insert name val (bound env) } cont other
  (LitP (StrL s)) (StrV t) | s == t -> inner
  (LitP (IntL s)) (IntV t) | s == t -> inner
  _ _ -> Eval $ \_ _ other -> other