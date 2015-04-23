{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE TypeFamilies      #-}

module Main (main) where

import           Control.Applicative
import           Control.Arrow
import           Control.Monad.State.Lazy
import qualified Ivory.Compile.C.CmdlineFrontend as C (compile)
import           Ivory.Language
import           Ivory.Language.Module

main :: IO ()
main = C.compile $ pure $ package "foo" $ runWithUniqueNames $ do
  lfo <- saw 0.00002
  r <- rect lfo
  x <- saw 0.016
  a <- add r x
  return a

type M a = StateT (Integer, [Def ('[] :-> ())]) ModuleM a

runWithUniqueNames :: M (Signal IFloat) -> ModuleM ()
runWithUniqueNames action = do
  (outputSignal, (_counter, processors)) <- runStateT action (0, [])
  jack_ivory_main processors outputSignal

unique :: String -> M String
unique pattern = do
  (counter, processors) <- get
  put (succ counter, processors)
  return (pattern ++ "_" ++ show counter)

addProcessor :: Def ('[] :-> ()) -> M ()
addProcessor p =
  modify (second (++ [p]))

mkProcessor :: String
  -> (Signal IFloat -> Body ())
  -> M (Signal IFloat)
mkProcessor pattern action = do
  outputRefName <- unique (pattern ++ "_output")
  let outputRef = area outputRefName (Just (ival 0))

  processorName <- unique (pattern ++ "_proc")
  let processor = proc processorName $ action $ Signal $ addrOf outputRef
  lift $ do
    defMemArea outputRef
    incl processor
  addProcessor processor

  return $ Signal $ addrOf outputRef

mkState :: (Ref Global (Stored IFloat) -> M a) -> M a
mkState action = do
  phaseRefName <- unique "rect_phase"
  let phaseRef = area phaseRefName (Just (ival 0))
  lift $ defMemArea phaseRef
  action $ addrOf phaseRef

jack_ivory_main :: [Def ('[] :-> ())] -> Signal IFloat -> ModuleM ()
jack_ivory_main processors (Signal input) =
  incl $ proc "jack_ivory_main" $ body $ do
    mapM_ call_ processors
    x <- deref input
    ret x

data Signal a = Signal (Ref Global (Stored a))

rect :: Signal IFloat -> M (Signal IFloat)
rect (Signal inc) = mkState $ \ phaseRef -> mkProcessor "rect" $ \ (Signal output) -> body $ do
  phase <- deref phaseRef
  incV <- deref inc
  let newPhase = (phase >=? 1) ? (phase - 1, phase)
      newPhase' = newPhase + incV
  store phaseRef newPhase'
  store output ((newPhase' <? 0.5) ? (- 1, 1 :: IFloat) * 0.1)

saw :: IFloat -> M (Signal IFloat)
saw inc = mkState $ \ phaseRef -> mkProcessor "saw" $ \ (Signal output) -> body $ do
  phase <- deref phaseRef
  let newPhase = (phase >=? 1) ? (phase - 1, phase)
      newPhase' = newPhase + inc
  store phaseRef newPhase'
  store output (newPhase' * 0.1)

add :: Signal IFloat -> Signal IFloat -> M (Signal IFloat)
add (Signal a) (Signal b) = mkProcessor "add" $ \ (Signal output) ->
  body $ do
    r <- (+) <$> deref a <*> deref b
    store output r
