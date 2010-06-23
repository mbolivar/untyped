--- Named and nameless terms for the untyped lambda calculus as in TAPL,
--- Ch. 6

module Term where

import Data.List (elemIndex, sort, elem)
import Data.Maybe (fromJust)

--- Types for terms

  -- TODO add boolean and numerical types/values; spread their evaluation
  -- throughout the interpreter.

-- Nameless terms
type Index = Integer
data NamelessTerm = NLAbs NamelessTerm
                  | NLApp NamelessTerm NamelessTerm
                  | NLVar Index
                  deriving Eq

instance Show NamelessTerm where
  show (NLAbs t) = "(\\. " ++ show t ++ ")"
  show (NLApp t1 t2) = "(" ++ show t1 ++ " " ++ show t2 ++ ")"
  show (NLVar i) = show i

-- Named terms
type VarName = String
data Term = Abs VarName Term
          | App Term Term
          | Var VarName
            
instance Show Term where
  show (Abs var t) = "(\\" ++ var ++ ". " ++ show t ++ ")"
  show (App t1 t2) = "(" ++ show t1 ++ " " ++ show t2 ++ ")"
  show (Var v) = v

instance Eq Term where
  t1 == t2 = (fst $ removeNames t1) == (fst $ removeNames t2)
  
-- Naming contexts.  For convenience, our variable->index conversion is the
-- _OPPOSITE_ of that in TAPL; e.g., the variable with index zero is at the
-- _HEAD_ of the list.
newtype Context = Context [VarName] deriving Show

emptyContext = Context []

nameIndex :: VarName -> Context -> Index
nameIndex name (Context ctx) = fromIntegral $ fromJust $ elemIndex name ctx

pushName :: VarName -> Context -> Context
pushName name (Context ctx) = Context $ name:ctx

--- Operations on terms
  
freeVariables :: Term -> [VarName]
freeVariables t = helper t [] -- (i know i should be using sets)
  where helper (Abs v b) bindings = helper b $ v:bindings
        helper (App t1 t2) bindings =
          (helper t1 bindings) ++ (helper t2  bindings)
        helper (Var v) bindings = if v `elem` bindings then [] else [v]

canonicalContext :: Term -> Context
canonicalContext t = Context $ sort $ freeVariables t

removeNamesWithCtx t ctx = 
  case t of
    (Abs v b) -> NLAbs $ removeNamesWithCtx b (pushName v ctx)
    (App t1 t2) -> NLApp (removeNamesWithCtx t1 ctx) $ 
                         (removeNamesWithCtx t2 ctx)
    (Var v) -> NLVar $ nameIndex v ctx

removeNames :: Term -> (NamelessTerm, Context)
removeNames t = (removeNamesWithCtx t ctx, ctx)
  where ctx = canonicalContext t

