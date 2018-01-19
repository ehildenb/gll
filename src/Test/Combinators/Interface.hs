module Test.Combinators.Interface where

import Distribution.TestSuite

import GLL.Combinators.Interface ( AltExpr , Parseable , parse , satisfy )
import GLL.Parseable.Char ()

testParser :: (Show t, Ord t, Parseable t, Eq a) => String -> AltExpr t a -> [([t], [a])] -> [TestInstance]
testParser name parser arg_pairs = let testInstance arg_pair = TestInstance { run       = return (Finished $ runTest arg_pair)
                                                                            , name      = "test: " ++ name
                                                                            , tags      = []
                                                                            , options   = []
                                                                            , setOption = \_ _ -> Right $ testInstance arg_pair
                                                                            }
                                    in fmap testInstance arg_pairs
    where
        runTest (input, output) = case parse parser input of
                                    output' | output == output' -> Pass
                                    _                           -> Fail name

tests :: IO [Test]
tests = return $ fmap Test $ testParser "eps1" (satisfy 0) [("", [0])]
