main = do
        nom <- getLine
        putStrLn (getMsg nom)

getMsg nom
    | (lc == 'a') || (lc == 'A') = "Hola maca!"
    | otherwise = "Hola maco!"
    where
        lc = lastChar nom

lastChar s = s !! (length s - 1)
