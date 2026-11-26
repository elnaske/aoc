parseInput :: String -> Int
parseInput word = do
  let num = read $ drop 1 word
  if head word == 'L'
    then (-1) * num
    else num

rotateDial :: Int -> Int -> Int
rotateDial startPosition turnAmount = (startPosition + turnAmount) `mod` 100

-- scanl rotateDial gets the position of the dial after each step
-- length $ filter (0==) counts the number of times the dial landed on 0
main = interact $ show . length . filter (0==) . scanl rotateDial 50 . map parseInput . words
