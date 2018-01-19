{-| This model contains unit-tests for 'GLL.Combinators.Interface'.

= Included examples

  * Elementary parsers
  * Sequencing
  * Alternatives
  * Simple binding
  * Binding with alternatives
  * Recursion (non-left)

  * Higher-order patterns:

      * Optional
      * Kleene-closure / positive closure
      * Seperator
      * Inline choice

  * Ambiguities:

      * "aaa"
      * longambig
      * aho_s
      * EEE

  * Left recursion
  * Hidden left-recursion
-}

module Test.Combinators.Interface where

import Distribution.TestSuite

import Data.Char ( ord )

import GLL.Combinators.Interface
import GLL.Parseable.Char ()

testParser :: (Show t, Ord t, Parseable t, Eq a, IsSymbExpr e) => String -> e t a -> [([t], [a])] -> [TestInstance]
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

aho_S_5 :: [String]
aho_S_5 = ["10101010100","10101011000","10101100100","10101101000","10101110000","10110010100","10110011000","10110100100","10110101000","10110110000","10111000100","10111001000","10111010000","10111100000","11001010100","11001011000","11001100100","11001101000","11001110000","11010010100","11010011000","11010100100","11010101000","11010110000","11011000100","11011001000","11011010000","11011100000","11100010100","11100011000","11100100100","11100101000","11100110000","11101000100","11101001000","11101010000","11101100000","11110000100","11110001000","11110010000","11110100000","11111000000"]

_EEE_3 :: [String]
_EEE_3 = ["00111","01011","01101","01110","10011","10101","10110","11001","11010","111","11100"]

tests :: IO [Test]
tests = return $ fmap Test

               --- Elementary parsers
               $ ( testParser "eps1"    (satisfy 0)      [("", [0])]
                ++ testParser "eps2"    (satisfy 0)      [("", [0]), ("111", [])]
                ++ testParser "single"  (char 'a')       [("a", ['a']), ("abc", [])]
                ++ testParser "semfun1" (1 <$$ char 'a') [("a", [1])]

               --- Elementary combinators
                ++ testParser "<**>" ((\b -> ['1',b]) <$$ char 'a' <**> char 'b') [("ab", ["1b"]), ("b", [])]

               --- Alternation
                ++ testParser "<||>" (ord <$$ char 'a' <**> char 'b' <||> ord <$$> char 'c') [("a", []), ("ab", [98]), ("c", [99]), ("cab", [])]

               --- Simple binding
                ++ let pX = "X" <:=> ord <$$> char 'a' <** char 'b'
                    in testParser "<:=>" pX [("ab", [97]), ("a", [])]

               --- Simple binding
                ++ let pX = "X" <::=> ord <$$> char 'a' <** char 'b'
                    in testParser "<::=>" pX [("ab", [97]), ("a", [])]

                ++ let pX = "X" <:=> flip (:) <$$> pY <**> char 'a'
                       pY = "Y" <:=> (\x y -> [x,y]) <$$> char 'b' <**> char 'c'
                    in testParser "<::=> 2" pX [("bca", ["abc"]), ("cba", [])]

               --- Binding with alternatives
                ++ let pY = "Y" <::=> char 'a' <||> char 'b'
                       pX = "X" <::=> pY <** char 'c'
                    in testParser "<::=> <||>" pX [("ac", "a"), ("bc", "b")]

               --- (Right) Recursion
                ++ let pX = "X" <::=> (+1) <$$ char 'a' <**> pX <||> satisfy 0
                    in testParser "rec1" pX [("", [0]), ("aa",[2]), (replicate 42 'a', [42]), ("bbb", [])]

               --- EBNF
                ++ let pX = "X" <::=> id <$$ char 'a' <** char 'b' <**> optional (char 'z')
                    in testParser "optional" pX [("abz", [Just 'z']), ("abab", []), ("ab", [Nothing])]

                ++ let pX = "X" <::=> (char 'a' <||> char 'b')
                    in testParser "<||> optional" (pX <** optional (char 'z')) [("az", "a"), ("bz", "b"), ("z", []), ("b", "b"), ("a", "a")]

                ++ let pX = "X" <::=> (1 <$$ optional (char 'a') <||> 2 <$$ optional (char 'b'))
                    in testParser "optional-ambig" (pX <** optional (char 'z')) [("az", [1]), ("bz", [2]), ("z", [1,2]), ("b", [2]), ("a", [1])]

                ++ let pX = "X" <::=> id <$$ char 'a' <**> (char 'b' <||> char 'c')
                    in testParser "inline choice (1)" pX [("ab", "b"), ("ac", "c"), ("a", []), ("b", [])]

                ++ let pX = "X" <::=> length <$$> multiple (char '1')
                    in testParser "multiple" pX [("", [0]), ("11", [2]), (replicate 12 '1', [12])]

                ++ let pX = "X" <::=> length <$$> multiple1 (char '1')
                    in testParser "multiple1" pX [("", []), ("11", [2]), (replicate 12 '1', [12])]

                ++ let pX = "X" <::=> 1 <$$ multiple (char 'a') <||> 2 <$$ multiple (char 'b')
                    in testParser "(multiple <||> multiple) <**> optional" (pX <** optional (char 'z')) [("az", [1]), ("bz", [2]), ("z", [1,2]), ("", [1,2]), ("b", [2]), ("a", [1])]

                ++ let pX = "X" <::=> pY <** optional (char 'z')
                       pY = "Y" <::=> length <$$> multiple (char 'a')
                                 <||> length <$$> multiple1 (char 'b') <** char 'e'
                    in testParser "multiple & multiple1 & optional" pX [("aaaz", [3]), ("bbbez", [3]), ("ez", []), ("z", [0]), ("aa", [2]), ("bbe", [2])]

               --- multiple with nullable argument
                ++ let pX = 1 <$$ char '1' <||> satisfy 0
                    in testParser "multiple (nullable arg)" (multiple pX) [("11", [[1,1]]), ("", [[]]), ("e", [])]

               --- Simple ambiguities
                ++ let pX = (++) <$$> pA <**> pB
                       pA = "a" <$$ char 'a' <||> "aa" <$$ char 'a' <** char 'a'
                       pB = "b" <$$ char 'a' <||> "bb" <$$ char 'a' <** char 'a'
                    in testParser "aaa" pX [("aaa", ["aab", "abb"]), ("aa", ["ab"])]

                ++ let pX = (\x y -> [x,y]) <$$ char 'a' <**> pL <**> pL <** char 'e'
                       pL =     1 <$$ char 'b'
                           <||> 2 <$$ char 'b' <** char 'c'
                           <||> 3 <$$ char 'c' <** char 'd'
                           <||> 4 <$$ char 'd'
                    in testParser "longambig" pX [("abcde", [[1,3], [2,4]]), ("abcdd", [])]

                ++ let pX = "X" <::=> (1 <$$ multiple1 (char 'a') <||> 2 <$$ multiple (char 'b'))
                       pY = "Y" <::=> (+) <$$> pX <**> pY <||> satisfy 0
                    in testParser "multiple1 & multiple & recursion + ambiguities" pY [("ab", [3]),("aa", [1,2]), (replicate 10 'a', [1..10])]

               --- TODO: shouldn't this be 1 + infinite 0's?
                ++ let pX = "X" <::=>  1 <$$ char 'a' <||> satisfy 0
                       pY = "Y" <::=> (+) <$$> pX <**> pY
                    in testParser "no parse infinite rec?" pY [("a", [])]

                ++ let pS = "S" <::=> ((\x y -> x+y+1) <$$ char '1' <**> pS <**> pS) <||> satisfy 0
                    in testParser "aho_S" pS [("", [0]), ("1", [1]), (replicate 5 '1', [5])]

                ++ let pS = "S" <::=> ((\x y -> '1':x++y) <$$ char '1' <**> pS <**> pS) <||> satisfy "0"
                    in testParser "aho_S" pS [("", ["0"]), ("1", ["100"]), ("11", ["10100", "11000"]) ,(replicate 5 '1', aho_S_5)]

                ++ let pE = "E" <::=> (\x y z -> x+y+z) <$$> pE <**> pE <**> pE <||> 1 <$$ char '1' <||> satisfy 0
                    in testParser "EEE" pE [("", [0]), ("1", [1]), ("11", [2]) ,(replicate 5 '1', [5]), ("112", [])]

                ++ let pE = "E" <::=> (\x y z -> x++y++z) <$$> pE <**> pE <**> pE <||> "1" <$$ char '1' <||> satisfy "0"
                    in testParser "EEE ambig" pE [("", ["0"]), ("1", ["1"]) ,("11", ["110", "011", "101"]), ("111", _EEE_3)]

                ++ let pX = "X" <::=>  maybe 0 (const 1) <$$> optional (char 'z') <||> (+1) <$$> pX <** char '1'
                    in testParser "simple left-recursion" pX [("", [0]), ("z11", [3]), ("z", [1]) ,(replicate 100 '1', [100])]

                ++ let pX = "X" <::=> satisfy 0 <||> (+1) <$$ pB <**> pX <** char '1'
                       pB = maybe 0 (const 0) <$$> optional (char 'z')
                    in testParser "hidden left-recursion" pX [("", [0]), ("zz11", [2]), ("z11", [2]), ("11", [2]) ,(replicate 100 '1', [100])]

                ++ let pX = "X" <::=> (+) <$$> pY <**> pA
                       pA = 1 <$$ char 'a' <** char 'b' <||> satisfy 0
                       pY = "Y" <::=> satisfy 0 <||> pX
                    in testParser "hidden left-recursion + infinite derivations" pX [("", [0]), ("ab", [1]), ("ababab", [3])]

               --- Testing ambiguity reduction combinators
                ++ let pX = (++) <$$> pA <**>>> pB
                       pA = "a" <$$ char 'a' <||> "aa" <$$ char 'a' <** char 'a'
                       pB = "b" <$$ char 'a' <||> "bb" <$$ char 'a' <** char 'a'
                    in testParser "A<A" pX   [("aaa", ["aab"]),("aa", ["ab"])]

                ++ let pX = (++) <$$> pA <<<**> pB
                       pA = "a" <$$ char 'a' <||> "aa" <$$ char 'a' <** char 'a'
                       pB = "b" <$$ char 'a' <||> "bb" <$$ char 'a' <** char 'a'
                    in testParser "A>A" pX   [("aaa", ["abb"]),("aa", ["ab"])]

                ++ let pX = "X" <:=> multiple pY
                       pY = 1 <$$ char '1' <||> 2 <$$ char '1' <** char '1'
                    in testParser "multiple" pX [("", [[]]), ("1", [[1]]), ("11", [[1,1],[2]]), ("111", [[1,1,1], [2,1], [1,2]])]

                ++ let pX = "X" <:=> some pY
                       pY = 1 <$$ char '1' <||> 2 <$$ char '1' <** char '1'
                    in testParser "some" pX [("", [[]]), ("1", [[1]]), ("11", [[2]]), ("111", [[2,1]])]

                ++ let pX = "X" <:=> many pY
                       pY = 1 <$$ char '1' <||> 2 <$$ char '1' <** char '1'
                    in testParser "many" pX [("", [[]]), ("1", [[1]]), ("11", [[1,1]]), ("111", [[1,1,1]])]

                ++ let pX = "X" <:=> "1" <$$ char '1' <||> multipleSepBy (char '1') (char ';')
                    in testParser "multipleSepBy" pX [("", [""]), ("1", ["1", "1"]), ("1;1", ["11"])]

                -- pX matches epsilon, therefore leading to infinitely many derivations
                ++ let pX = "X" <::=> 1 <$$ char '1' <||> sum <$$> multipleSepBy pX (char ';')
                    in testParser "multipleSepBy2" pX [("", [0]), ("1", [1,1]), ("1;1", [2]), (";1", [1]),  (";1;1", [2])]

                ++ let pX = "X" <:=> length <$$> multiple (char '1')
                    in testParser "multiple1" pX [("", [0]), ("11", [2]), (replicate 10 '1', [10])]

                ++ let pX = "X" <:=> length <$$> multiple (char '1') <** char 'z'
                    in testParser "multiple2" pX [("", []), ("11z", [2]), (replicate 10 '1' ++ "z", [10])]

                ++ let pX   = "X" <:=> length <$$> multiple pEps <** char 'z'
                       pEps = satisfy () <||> () <$$ char '1'
                    in testParser "multiple & epsilon" pX [("", []), ("z", [0])]

               --- why not ("", [[0]]) ??
                ++ let pX = "X" <::=> 1 <$$ char '1' <||> sum <$$> multipleSepBy pX (char ';')
                    in testParser "multipleSepBy and multiple" (multiple pX) [("", [[]]), ("1", [[1],[1]]), ("1;1", [[1,0,1],[1,1],[2]]) ,(";1;1", [[0,1,0,1],[0,1,1], [0,2], [1,0,1], [1,1], [2]])]
                 )

--- {-
---     -- a combinator `fewest` (variant of multiple) should behave as follows
---     ++ let pX = "X" <:=> fewest pY
---          where pY = 1 <$$ char '1' <||> 2 <$$ char '1' <** char '1'
---         in testParser "some" pX
---       [("", [[]]), ("1", [[1]]), ("11", [[2]]), ("111", [[2,1], [1,2]])]
---
---     ++ let pX :: BNF Char Int
---         pX = "X" <::=> 1 <$$ char '1' <||> sum <$$> multipleSepBy pX (char ';')
---         in testParser "manySepBy and multiple" (many pX)
---       -- why not ("", [[0]]) ??
---       [("", [[]]), ("1", [[1],[1]]), ("1;1", [[1,0,1]])]-}
--- -}

---     putStrLn "Tests that use memoisation"
---
---     let tab = newMemoTable
---         pX = "X" <::=> (1 <$$ multiple1 (char 'a') <||> 2 <$$ multiple (char 'b'))
---         pY = memo tab ("Y" <::=> (+) <$$> pX <**> pY
---                    <||> satisfy 0)
---     test (Just tab) "multiple1 & multiple & recursion + ambiguities" pY
---         [("ab", [3]),("aa", [1,2]), (replicate 10 'a', [1..10])]
---
---     let tab = newMemoTable
---         pX = "X" <::=>  1 <$$ char 'a' <||> satisfy 0
---         pY = memo tab ("Y" <::=> (+) <$$> pX <**> pY)
---     -- shouldn't this be 1 + infinite 0's?
---     test (Just tab) "no parse infinite rec?" pY
---         [("a", [])]
---
---     --  Higher ambiguities
---     let tab = newMemoTable
---         pE = memo tab ("E" <::=> (\x y z -> x+y+z) <$$> pE <**> pE <**> pE
---                              <||> 1 <$$ char '1'
---                              <||> satisfy 0)
---     test (Just tab) "EEE" pE [("", [0]), ("1", [1]), ("11", [2])
---                              ,(replicate 5 '1', [5]), ("112", [])]
---
---     let tab = newMemoTable
---         pX = "X" <::=> (+) <$$> pY <**> pA
---         pA = 1 <$$ char 'a' <** char 'b' <||> satisfy 0
---         pY = memo tab ("Y" <::=> satisfy 0 <||> pX)
---     test (Just tab) "hidden left-recursion + infinite derivations" pX
---         [("", [0]), ("ab", [1]), ("ababab", [3])]
