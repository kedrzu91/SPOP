
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

solvePuzzle :: ([[Int]], [[Int]]) -> [[Bool]]
solvePuzzle x = [[False, True], [True, False]]

data Cell = Empty | Black | White | Unknown
                deriving Show


solvePuzzle2 (rows, cols) = let table = createTable length(cols)  length(rows)



createTable Int-> Int-> [[Cell]]
createTable colN rowN = [ [Empty | x<-[1...colN]] | xs <-[1..rowN] ]
createRow colN = [Empty | x<-[1...colN]]

solve:: ([[Cell]], [Int], [[Int]])

solveRows:: ([[Cell]], [Int], [[Int]], [[Int]]) -> [[Cell]]
solveRows (game, rows, rowsGr, colsGr)

solveCols:: ([[Cell]], [Int], [[Int]], [[Int]])
solveCols (game, cols, rowsGr, colsGr)

trySolveRow :: ([Cell], [Int]) -> ([Cell], [Int])

trySolveRow  (game, []) = (game, [])
trySolveRow  (game, rowGr) = let refRow = createRow (length game)
                                 firstSov = genFirst rowGr
                                 refRow2 = trySolveRow2 (game, rowGr, refRow, firstSov)
                             in  (refRow2 , diff refRow2 game)


genFirst::[Int] -> [Int]
genFirst list = 1:(genFirst2 1 list)
genFirst2 (idx, x:xs) = let new = (idx + x + 1)
                        in new:(genFirst2 new xs)

trySolveRow2 :: ([Cell], [Int], [Cell], Maybe [Int]) -> ([Cell]
trySolveRow2  (game, rowGr, refRow, Nothing) = sum game refRow
trySolveRow2  (game, rowGr, refRow, curSol) =
                if isPossible game curSol then
                    let newRefRow = andRow (refRow, curSol)
                        newCurSol = genNextSol curSol rowGr
                    in trySolveRow2  (game, rowGr, newRefRow, newCurSol)
                else
                    let newCurSol = genNextSol curSol rowGr
                    in trySolveRow2  (game, rowGr, refRow, newCurSol)

genNextSol curSol rowGr rowSize =  if (last curSol) + (last rowGr) > rowSize then
                                          genNextSol2 (take ((length curSol) - 1 ) curSol) rowGr rowSize

genNextSol2 curSol rowGr rowSize =

trySolveCol :: ([Cell], Int, [Int]) -> ([[Cell]], [Int])
trySolveCol =



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
