-- Parsers for producing named and nameless untyped LC terms

module Parse where

import Data.Maybe (fromJust)
import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as T
import qualified Text.ParserCombinators.Parsec.Language as L

import Term

trim = unwords . words          -- WTF why doesn't this exist already?

-- Parsing functions

parseUntypedNameless :: String -> Maybe NamelessTerm
parseUntypedNameless input =
  case parse untypedNLTermSeqToApp "" $ trim input of
    Left err -> Nothing
    Right t -> Just t
  
parseUntypedNameless' :: String -> NamelessTerm
parseUntypedNameless' = fromJust . parseUntypedNameless

parseUntyped :: String -> Maybe Term
parseUntyped input =
  case parse untypedTermSeqToApp "" $ trim input of
    Left err -> Nothing
    Right t -> Just t
    
parseUntyped' :: String -> Term
parseUntyped' = fromJust . parseUntyped

-- Parsers

seqJoiner :: Parser a -> (a -> a -> a) -> Parser a
seqJoiner p j = do {terms@(t:ts) <- sepBy1 p spaces;
                    return $ foldl j t ts}

untypedNLTerm :: Parser NamelessTerm
untypedNLTerm = 
  do { char '('; t <- untypedNLTermSeqToApp; char ')'; return t}
  <|> do {n <- number; return $ NLVar n}
  <|> do {string "\\."; spaces;
          t <- untypedNLTermSeqToApp;
          return $ NLAbs t}
    where number = T.decimal L.haskell
          
untypedNLTermSeqToApp :: Parser NamelessTerm          
untypedNLTermSeqToApp = seqJoiner untypedNLTerm NLApp

untypedTerm :: Parser Term
untypedTerm =
  do { char '('; t <- untypedTermSeqToApp; char ')'; return t}
  <|> do { v <- varName; return $ Var v}
  <|> do {string "\\"; 
          v <- varName; 
          string "."; spaces;
          body <- untypedTermSeqToApp;
          return $ Abs v body}
  where varName = T.identifier L.haskell

untypedTermSeqToApp :: Parser Term
untypedTermSeqToApp = seqJoiner untypedTerm App
