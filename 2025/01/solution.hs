import Test.HUnit

-- TODO: read and parse input file

rotate pos amount = (pos + amount) `rem` 100

solution input = length $ filter (0==) $ scanl (rotate) 50 input

main :: IO Counts
main = runTestTT $ tests

tests :: Test
tests = do
    let input = [-68, -30, 48, -5, 60, -55, -1, -99, 14, -82]
    TestCase (solution input @?= 3)
