
module Test.Utils where

import           Control.Applicative
import qualified Ivory.Compile.C.CmdlineFrontend as C (compile)
import           Ivory.Language
import           System.IO.Silently
import           System.Process
import           Test.Hspec
import           Test.Mockery.Directory

import           Run

compareToList :: M (Signal IFloat) -> Int -> [String] -> Expectation
compareToList processor drops expected =
  execute drops processor `shouldReturn` expected

(===) :: M (Signal IFloat) -> M (Signal IFloat) -> Expectation
a === b = do
  a' <- execute 100 a
  b' <- execute 100 b
  a' `shouldBe` b'

execute :: Int -> M (Signal IFloat) -> IO [String]
execute drops processor =
  inTempDirectory $ do
    C.compile $ pure $ package "proc" $ runWithUniqueNames processor
    writeFile "main.c" ("\n\
      \#include <stdio.h>\n\
      \#include \"proc.h\"\n\
      \ int main() {\n\
      \   int i;\n\
      \   for (i; i < " ++ show drops ++ "; i++) {\n\
      \     /* printf(\"%f \", (float) 32.0f); */ \n\
      \     printf(\"%f \", jack_ivory_main());\n\
      \   }\n\
      \ }")
    system "gcc proc.c main.c"
    words <$> capture_ (system "./a.out")
