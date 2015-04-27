{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE TypeFamilies      #-}

module Run where

import           Control.Applicative
import           Control.Arrow
import           Control.Monad.State.Lazy
import qualified Ivory.Compile.C.CmdlineFrontend as C (compile)
import           Ivory.Language
import           Ivory.Language.Module

run :: IO ()
run = C.compile $ pure $ package "foo" $ runWithUniqueNames $ do
  rs <- forM (map (\ i -> 0.00002 * fromIntegral i) [1 .. 3 :: Integer]) $
    \ inc -> do
      lfo <- join (mult <$> (saw inc) <*> (constant 0.1))
      rect lfo
  siren <- join (mult <$> constant 0.3 <*> mix rs)
  bassdrum <- do
    kick <- rect =<< constant 0.00002
    ramp <- saw 0.000001
    mult kick ramp
  mix [siren, bassdrum, bassdrum]


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

mkProcessor :: (IvoryInit a, Num a) =>
     String
  -> (Signal a -> Body ())
  -> M (Signal a)
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
jack_ivory_main processors (Signal input) = incl inner
  where
    inner :: Def ('[] :-> IFloat)
    inner = proc "jack_ivory_main" $ body $ do
      mapM_ call_ processors
      x <- deref input
      ret x

data Signal a = Signal (Ref Global (Stored a))

rect :: Signal IFloat -> M (Signal IFloat)
rect (Signal inc) = mkState $ \ phaseRef -> mkProcessor "rect" $
  \ (Signal output) -> body $ do
    phase <- deref phaseRef
    incV <- deref inc
    let newPhase = phase + incV
        newPhase' = (newPhase >=? 1) ? (newPhase - 1, newPhase)
    store phaseRef newPhase'
    store output ((phase <? 0.5) ? (- 1, 1 :: IFloat))

saw :: IFloat -> M (Signal IFloat)
saw inc = mkState $ \ phaseRef -> mkProcessor "saw" $
  \ (Signal output) -> body $ do
    phase <- deref phaseRef
    let newPhase = (phase >=? 1) ? (phase - 1, phase)
        newPhase' = newPhase + inc
    store phaseRef newPhase'
    store output newPhase'

add :: Signal IFloat -> Signal IFloat -> M (Signal IFloat)
add (Signal a) (Signal b) = mkProcessor "add" $ \ (Signal output) ->
  body $ do
    r <- (+) <$> deref a <*> deref b
    store output r

mix :: [Signal IFloat] -> M (Signal IFloat)
mix signals = mkProcessor "mix" $ \ (Signal out) -> body $ do
    values <- mapM (\ (Signal ref) -> deref ref) signals
    store out (sum values / len)
  where
    len :: IFloat
    len = fromIntegral $ length signals

constant :: (IvoryInit a, Num a, IvoryStore a) =>
  a -> M (Signal a)
constant c = mkProcessor "constant" $ \ (Signal out) -> body $ do
  store out c

mult :: (IvoryInit a, Num a, IvoryStore a) =>
  Signal a -> Signal a -> M (Signal a)
mult (Signal a) (Signal b) = mkProcessor "mult" $ \ (Signal out) -> body $ do
  a' <- deref a
  b' <- deref b
  store out (a' * b')

volume :: IFloat -> Signal IFloat -> M (Signal IFloat)
volume factor input =
  join (mult <$> constant factor <*> pure input)
