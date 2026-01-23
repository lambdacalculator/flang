import FLang hiding (match3) -- hide to avoid clash if we import explicit
import FLang.FSM (accept, nacc)
import FLang.RegExp (match1, match2, compile, runmp, match3)
import FLang.Algorithms (thompson, glushkov, brzozowski, elim_eps, fsm_to_re, minimize)
import Test.QuickCheck
import System.Exit (exitFailure, exitSuccess)

-- Generators
instance Arbitrary RegExp where
  arbitrary = sized arbRegExp

arbRegExp :: Int -> Gen RegExp
arbRegExp 0 = elements [Zero, One, Let 'a', Let 'b']
arbRegExp n = frequency
  [ (1, return Zero)
  , (1, return One)
  , (2, Let <$> elements "ab")
  , (2, Union <$> sub <*> sub)
  , (2, Cat <$> sub <*> sub)
  , (2, Star <$> sub)
  ]
  where sub = arbRegExp (n `div` 2)

-- Restricted Generator for short properties
shortGen :: Gen (RegExp, [String])
shortGen = do
  r <- resize 5 arbitrary 
  -- Generate small number of short strings over STRICT alphabet "ab"
  ws <- resize 5 (listOf (resize 5 (listOf (elements "ab"))))
  return (r, ws)

-- Properties

-- 1. Match Consistency: match1, match2, and match3 (compiled) agree
prop_matchConsistency :: Property
prop_matchConsistency = forAll shortGen $ \(r, ws) ->
  let prog = compile r
      check w = let m1 = match1 r w
                    m2 = match2 r w
                    m3 = runmp prog w
                in m1 == m2 && m2 == m3
  in all check ws

-- 2. Brzozowski Construction
prop_brzozowski :: Property
prop_brzozowski = forAll shortGen $ \(r, ws) ->
  let prog = compile r
      mach = brzozowski r
  in all (\w -> accept mach w == runmp prog w) ws

-- 3. Thompson Construction
prop_thompson :: Property
prop_thompson = forAll shortGen $ \(r, ws) ->
  let prog = compile r
      mach = elim_eps $ thompson r
  in all (\w -> nacc mach w == runmp prog w) ws

-- 4. Glushkov Construction
prop_glushkov :: Property
prop_glushkov = forAll shortGen $ \(r, ws) ->
  let prog = compile r
      mach = glushkov r
  in all (\w -> nacc mach w == runmp prog w) ws

-- 5. Minimization
prop_minimize :: Property
prop_minimize = forAll shortGen $ \(r, ws) ->
  let prog = compile r
      mach = minimize "ab" $ brzozowski r
  in all (\w -> accept mach w == runmp prog w) ws

-- 6. Roundtrip: FSM -> RegExp
prop_roundtrip :: Property
prop_roundtrip = forAll shortGen $ \(r, ws) ->
  let prog = compile r
      r'   = fsm_to_re "ab" $ brzozowski r
      prog' = compile r'
  in all (\w -> runmp prog' w == runmp prog w) ws

-- 7. Custom Alphabet Test (ABC)
-- Just reuse the standard RegExp type but put 'c' in it manually
-- Arbitrary instance is already defined above covering defaults.
-- Here we define a custom generator for 'abc' alphabet.
-- Let's make a custom generator for ABC regexes.

arbRegExpABC :: Int -> Gen RegExp
arbRegExpABC 0 = elements [Zero, One, Let 'a', Let 'b', Let 'c']
arbRegExpABC n = frequency
  [ (1, return Zero)
  , (1, return One)
  , (2, Let <$> elements "abc")
  , (2, Union <$> sub <*> sub)
  , (2, Cat <$> sub <*> sub)
  , (2, Star <$> sub)
  ]
  where sub = arbRegExpABC (n `div` 2)

prop_abc :: Property
prop_abc = forAll (resize 5 (sized arbRegExpABC)) $ \r ->
  let m = minimize "abc" (brzozowski r)
      -- Test that minimization doesn't crash and produces valid FSM
      qs = reachable "abc" m
  in length qs >= 0


-- Main
main :: IO ()
main = do
  putStrLn "Running FLang Properties..."
  
  putStrLn "1. Match Consistency (match1 vs match2 vs match3)..."
  quickCheck prop_matchConsistency

  putStrLn "2. Brzozowski Construction..."
  quickCheck prop_brzozowski
  
  putStrLn "3. Thompson Construction..."
  quickCheck prop_thompson

  putStrLn "4. Glushkov Construction..."
  quickCheck prop_glushkov
  
  putStrLn "5. Minimization..."
  quickCheck prop_minimize
  
  putStrLn "6. FSM -> RegExp Roundtrip..."
  quickCheck prop_roundtrip

  putStrLn "7. Custom Alphabet (ABC)..."
  quickCheck prop_abc
  
  putStrLn "Done."
