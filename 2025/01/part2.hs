parseInput :: String -> Int
parseInput w = do
  let num = read $ drop 1 w
  if head w == 'L'
    then (-1) * num
    else num

rotateDial :: Int -> Int -> Int
rotateDial pos amount = (pos + amount) `mod` 100

timesPointedAtZero :: Int -> Int -> Int
timesPointedAtZero pos amount
  | pos == 0 = (abs amount) `div` 100
  | amount >= 0 = (pos + amount) `div` 100
  | amount < 0 = (100 - (pos + amount)) `div` 100

solve :: Int -> [Int] -> Int
solve pos turns =
  if null turns
    then 0
    else timesPointedAtZero pos (head turns) + solve (rotateDial pos (head turns)) (drop 1 turns)

main = interact $ show . solve 50 . map parseInput . words
