module Nonogram where
import Control.Exception
import System.IO
import System.IO.Error

-- READ DATA FROM FILE

readData :: IO ([[Int]], [[Int]])
readData = catch (do fname <- getLine
                     handle <- openFile fname ReadMode
                     rowstr <- hGetLine handle
                     colstr <- hGetLine handle
                     let cols = (read colstr) :: [[Int]]
                         rows = (read rowstr) :: [[Int]]
                     hClose handle
                     return (rows, cols)
                  ) errorHandler
                      where errorHandler e = if isDoesNotExistError e 
                            then do putStrLn ("Nie istnieje plik o podanej nazwie")
                                    ioError e
                            else ioError e


-- SOLVE NONOGRAM

findLines :: [Int] -> Int -> [[Bool]]
findLines [] _ = []
findLines list n = []--findShortLines [] list n

-- 1 ([Bool]) - line beginning
-- 2 ([Int]) - uzupelnienie
-- 3 (Int) - ile miejsca na uzupelnienie
findShortLines :: [Bool] -> [Int] -> Int -> (Maybe [[Bool]])
findShortLines bs [] _ = Just [bs]
findShortLines _ _ 0 = Nothing
findShortLines bs list n = Nothing --TODO

solvePuzzle :: ([[Int]], [[Int]]) -> [[Bool]]
solvePuzzle x = [[False, True], [True, False]]

-- DRAW RESULT PICTURE

drawPicture :: [[Bool]] -> Int -> IO ()
drawPicture c n = do putChar '+'
                     drawBorderLine n
                     drawContent c n
                     putChar '+'
                     drawBorderLine n
                     return ()


drawBorderLine :: Int -> IO ()
drawBorderLine 0 = do putStrLn "+"
                      return ()

drawBorderLine n = do putChar '-'
                      drawBorderLine (n-1)
                      return ()


drawContent :: [[Bool]] -> Int -> IO ()
drawContent [] _ = return ()

drawContent (l:ls) n = do putChar '|'
                          drawLine l n
                          drawContent ls n
                          return ()


drawLine :: [Bool] -> Int -> IO ()
drawLine _ 0 = do putStrLn "|"
                  return ()

drawLine [] n = do putChar ' '
                   drawLine [] (n-1)
                   return ()

drawLine (b:bs) n = do if b == True then putChar 'x'
                           else putChar ' '
                       drawLine bs (n-1)
                       return ()

-- MAIN PROGRAM

main :: IO ()
main = catch (do putStr "Podaj nazwe pliku: "
                 puzzle <- readData
                 putStrLn "\nWiersze:"
                 print (fst puzzle)
                 putStrLn "\nKolumny:"
                 print (snd puzzle)
                 let solution = solvePuzzle puzzle
                 putStrLn "\nRozwiazanie:"
                 drawPicture solution (length (snd puzzle))
                 return ()
               ) errorHandler
               where errorHandler e =
                         if isDoesNotExistError e
                             then putStrLn ("Nie istnieje ")
                         else return ()
--           where errorHandler :: SomeException -> IO()
--                 errorHandler err = putStrLn $ "Caught exception: " ++ show err

{-
main = do putStr "Podaj nazwe pliku: "
          puzzle <- readData
          putStrLn "\nWiersze:"
          print (fst puzzle)
          putStrLn "\nKolumny:"
          print (snd puzzle)
          let solution = solvePuzzle puzzle
          putStrLn "\nRozwiazanie:"
          drawPicture solution (length (snd puzzle))
          return ()
-}
{-
xxxx = catch (print $ 5 `div` 0) handler
  where
    handler :: SomeException -> IO ()
    handler ex = putStrLn $ "Caught exception: " ++ show ex
    -}
