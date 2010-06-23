module Main where

import Data.Maybe (fromJust)

import Eval
import Parse
import Term

makeCombinator str = fst $ removeNames $ parseUntyped' str

s = makeCombinator "\\x.\\y.\\z. x z (y z)"
k = makeCombinator "\\x.\\y.x"

skk = NLApp (NLApp s k) k
      -- shit i forgot that skk only ==> id under full beta reduction oh well
      -- but isn't that cool that it does?
