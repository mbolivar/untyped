-- Call-by-value evaluator for the untyped lambda calculus, a la TAPL,
-- Ch. 6

module Eval where

import Data.List (takeWhile, iterate)
import Data.Maybe (fromJust)

import Term

-- HELL YEAH MOTHERFUCKERS
traceEval :: NamelessTerm -> [NamelessTerm]
traceEval t =
  t : (map fromJust $ takeWhile (not . (==Nothing)) $ iterate (>>=oneStep) $ oneStep t)

-- Call-by-value rules:
-- 
--    t1 -> t1'
-- -----------------
--  t1 t2 -> t1' t2
--
--    t2 -> t2'
-- -----------------
--  v1 t2 -> v1 t2'
--
--     t a normal form
-- -------------------------
-- (\x. b) t -> [x -> t] b
oneStep :: NamelessTerm -> Maybe NamelessTerm
oneStep t = case t of
  NLAbs _ -> Nothing
  NLVar _ -> Nothing
  NLApp t1 t2 -> oneStepApp t1 t2
  where oneStepApp :: NamelessTerm -> NamelessTerm -> Maybe NamelessTerm
        oneStepApp t1 t2 =
          case (t1, oneStep t1, oneStep t2) of
            (_, Just t1', _) -> Just $ NLApp t1' t2        -- t1 a redex
            (_, _, Just t2') -> Just $ NLApp t1 t2'        -- t2 a redex
            ((NLAbs b), _, Nothing) -> Just $ (0 |-> t2) b -- beta reduction
            _ -> Nothing                                   -- no rule applies

-- Shifting operation to keep variable numbers consistent under
-- substitution
upArrow :: Index -> Index -> NamelessTerm -> NamelessTerm
upArrow d c t = case t of
  NLVar k -> if k < c then NLVar k else NLVar $ k + d
  NLAbs b -> NLAbs $ upArrow d (c+1) b
  NLApp t1 t2 -> NLApp (upArrow d c t1) (upArrow d c t2)

-- The default shifting operation, where we assume we're starting at lambda
-- depth zero
shift :: Index -> NamelessTerm -> NamelessTerm
shift d t = upArrow d 0 t

-- This substitution operator is defined to make the implementation look
-- like the TAPL notation: when in TAPL you see [j |-> s]t, you can here
-- say (j |-> s) t.
(|->) :: Index -> NamelessTerm -> NamelessTerm -> NamelessTerm
j |-> s = \t -> case t of
  NLVar k -> if k == j then s else NLVar k
  NLAbs t1 -> NLAbs $ ((j+1) |-> (shift 1 s)) t1
  NLApp t1 t2 -> NLApp ((j |-> s) t1) ((j |-> s) t2)

            
  
