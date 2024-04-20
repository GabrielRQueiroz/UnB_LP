maior4 :: Int ->Int ->Int ->Int ->Int
maior4 a b c d
   | a >= b && a >= c && a >= d = a
   | b >= a && b >= c && b >= d = b
   | c >= a && c >= b && c >= d = c
   | otherwise = d

converterNotaParaMencao :: Float -> String
converterNotaParaMencao n
    | n >= 9 && n < 10 = "SS"
    | n >= 7 && n < 9 = "MS"
    | n >= 5 && n < 7 = "MM"
    | n >= 3 && n < 5 = "MI"
    | otherwise = "II"


main :: IO ()

main = putStrLn (show (maior4 10 20 3 4))