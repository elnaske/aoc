parseInput :: String -> Int
parseInput word = do
  let num = read $ drop 1 word
  if head word == 'L'
    then (-1) * num
    else num

rotateDial :: Int -> Int -> Int
rotateDial startPosition turnAmount = (startPosition + turnAmount) `mod` 100

-- In part 1 we determined the number of times the dial lands on 0 at the end
-- of the step.
-- Now we want to know how many times it _points_ at zero, including when it only crosses the zero mark.
timesPointedAtZero :: Int -> Int -> Int
timesPointedAtZero startPosition turnAmount
  | startPosition == 0 = (abs turnAmount) `div` 100
  | turnAmount >= 0 = (startPosition + turnAmount) `div` 100
  | turnAmount < 0 = (100 - (startPosition + turnAmount)) `div` 100

-- Instead of using scanl like in part 1, here we calculate the number of times
-- the dial lands on zero at each step using recursion
solve :: Int -> [Int] -> Int
solve startPosition turns =
  if null turns
    then 0
    else timesPointedAtZero startPosition (head turns) + solve (rotateDial startPosition (head turns)) (drop 1 turns)

main = interact $ show . solve 50 . map parseInput . words
