module Test.Combinators.Interface where

import Distribution.TestSuite

test :: (String, Result) -> TestInstance
test (name, res) = TestInstance { run       = return (Finished res)
                                , name      = "test: " ++ name
                                , tags      = []
                                , options   = []
                                , setOption = \_ _ -> Right $ test (name, res)
                                }

tests :: IO [Test]
tests = return [ Test $ test ("passes", Pass)
               , Test $ test ("fails", Fail "fail")
               ]
