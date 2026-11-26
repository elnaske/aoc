parseInput :: String -> Int
parseInput w = do
  let num = read $ drop 1 w
  if head w == 'L'
    then (-1) * num
    else num

rotateDial :: Int -> Int -> Int
rotateDial pos amount = (pos + amount) `mod` 100

main = interact $ show . length . filter (0==) . scanl rotateDial 50 . map parseInput . words
