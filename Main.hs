{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

import           Control.Applicative
import qualified Ivory.Compile.C.CmdlineFrontend as C (compile)
import           Ivory.Language

main :: IO ()
main = C.compile $ pure $ package "foo" $ do
  incl saw
  incl rect
  incl jack_main
  defMemArea val

printf :: Def ('[IString, Sint64] :-> ())
printf = importProc "printf" "stdio.h"

mainC :: Def ('[] :-> ())
mainC  = proc "main" $ body $ do
  x <- call gcd' 12 20
  call_ printf "result: %i\n" x
  retVoid

gcd' :: Def ('[Sint64, Sint64] :-> Sint64)
gcd' = proc "gcd_" $ \ a b -> body $ do
  ifte_ (a ==? b) (ret a) $ do
    ifte_ (a <=? b) (call gcd' b a) $ do
      call gcd' (a - b) b

rect :: Def ('[] :-> IFloat)
rect = proc "rect" $ body $ do
  phase <- deref (addrOf val)
  let newPhase = (phase >=? 1) ? (phase - 1, phase)
      newPhase' = newPhase + inc
  store (addrOf val) newPhase'
  ret ((newPhase' <? 0.5) ? (- 1, 1) * 0.1)

saw :: Def ('[] :-> IFloat)
saw = proc "saw" $ body $ do
  phase <- deref (addrOf val)
  let newPhase = (phase >=? 1) ? (phase - 1, phase)
      newPhase' = newPhase + inc
  store (addrOf val) newPhase'
  ret (newPhase' * 0.1)

jack_main :: Def ('[] :-> IFloat)
jack_main = proc "jack_ivory_main" $ body $ do
  a <- call rect
  b <- call saw
  ret (a + b)

inc :: IFloat
inc = 0.005

val :: MemArea (Stored IFloat)
val = area "val" (Just (ival 0))
