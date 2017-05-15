--import System.IO

-- index de massa corporal
indexMassa :: Float -> String
indexMassa x
    | x < 18.0 = "magror"
    | x < 25.0 = "corpulencia normal"
    | x < 30.0 = "sobrepes"
    | x < 40.0 = "obesitat"
    | otherwise = "obesitat morbida"

imc :: Float -> Float -> Float
imc w h = w / (h * h)


--getWord :: IO String
getWord = do c <- getChar
             if (c == '\n') || (c == ' ')
               then return ""
               else do w <- getWord
                       return (c:w)

process = do nom <- getWord
             if nom == "*" then return()
               else do
               w <- getWord
               h <- getWord
               putStr nom
               putStr ": "
               putStrLn $ indexMassa $ imc (read w :: Float) (read h :: Float)
               process

main = process
