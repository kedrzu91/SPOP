
import Control.Exception
import System.IO
import System.IO.Error
import Data.List

-- SOLVE NONOGRAM

data Cell = Empty | Black | White | Unknown
                deriving (Eq, Show)

createTable:: Int -> Int-> [[ Cell ]]
createTable colN rowN = [ (createRow colN) | xs <-[1..rowN] ]
createRow colN = [Empty | x<-[1..colN]]

solvePuzzle :: ([[Int]], [[Int]]) -> [[Bool]]


solvePuzzle x = convertGame (solvePuzzle2 x)

-- ta funkcja jest do uzupełnienia - trzeba dopisać żeby iterować na zmianę po kolumnach i wierszach
-- oraz wykorzystaćwyjście z funkcji trySolveRow : pierwszy element krotki to rozwiązany wiersz,
--  drugi to zmienione kolumny w wiersz
solvePuzzle2 :: ([[Int]], [[Int]]) -> [[Cell]]
solvePuzzle2 (rows, cols) = let game = createTable (length cols) (length rows)
                                solRows = solveGameByRow game rows
                                trGame = transpose solRows

                                solCols = solveGameByRow trGame cols
                                trGame2 = transpose solCols

                                solRows2 = solveGameByRow trGame2 rows
                                trGame3 = transpose solRows2

                                solCols2 = solveGameByRow trGame3 cols
                                trGame4 = transpose solCols2

                                in trGame4

solveGameByRow:: [[Cell]]->[[Int]]->[[Cell]]
solveGameByRow game rows = zipWith (\x y -> fst (trySolveRow x y)) game rows

convertGame :: [[Cell]]-> [[Bool]]
convertGame game = map convertRow game

convertRow :: [Cell]-> [Bool]
convertRow row = map (\x -> x == Black) row

trySolveRow game rowGr = let refRow = createRow (length game)
                             firstSov = genFirst rowGr
                             refRow2 = trySolveRow2 game rowGr refRow (Just firstSov)
                             newRow = unknownToEmpty refRow2
                             in  (newRow , (diff newRow game))

diff _ [] = []
diff [] _ = []
diff (x:xs) (y:ys) = (x /= y):(diff xs ys)

unknownToEmpty [] = []
unknownToEmpty (Unknown:list) = Empty:(unknownToEmpty list)
unknownToEmpty (x:list) = x:(unknownToEmpty list)

trySolveRow2 :: [Cell]-> [Int]->[Cell]-> Maybe [Int] -> [Cell]
trySolveRow2  gameRow rowGr refRow Nothing = andRow gameRow refRow
trySolveRow2  gameRow rowGr refRow (Just curSol) =
                if isPossible gameRow rowGr curSol then
                    let newRefRow = andRow refRow (toRow (length gameRow) rowGr curSol)
                        newCurSol = genNextSol curSol rowGr (length gameRow)
                    in trySolveRow2  gameRow rowGr newRefRow newCurSol
                else
                    let newCurSol = genNextSol curSol rowGr (length gameRow)
                    in trySolveRow2  gameRow rowGr refRow newCurSol



isPossible:: [Cell]-> [Int]-> [Int] -> Bool
isPossible gameRow rowGr solution = let len = length gameRow
                                        newrow = toRow len rowGr solution
                                    in compareRow gameRow newrow

toRow::Int-> [Int]->[Int] -> [Cell]
toRow len rowGr solution = toRow2 rowGr solution 1 len

toRow2::[Int]-> [Int]->Int->Int->[Cell]
toRow2 [] [] idx len = genList White (len - idx + 1)
toRow2 (r:rowGr) (s:solution) currIdx len = let white = genList White (s-currIdx)
                                                black = genList Black r
                                                rest = toRow2 rowGr solution (s+r) len
                                            in white ++ black ++ rest

genList value 0 = []
genList value len = [value | _ <- [1..len]]

compareRow::[Cell]-> [Cell] -> Bool
compareRow _ [] = True
compareRow [] _ = True
compareRow (Empty:gs) (r:rs) = compareRow gs rs
compareRow (g:gs) (r:rs) = if g == r then compareRow gs rs else False

andRow ::[Cell]->[Cell]->[Cell]
andRow [] [] = []
andRow (r1:row1) (r2:row2) = (sumCell r1 r2):(andRow row1 row2)

sumCell Unknown _ = Unknown
sumCell _ Unknown = Unknown
sumCell Empty x = x
sumCell x Empty = x
sumCell x y = if x == y then x else Unknown

-- Generacja nastepnej solucji

genFirst::[Int] -> [Int]
genFirst list = 1:(genFirst2 1 list)

genFirst2::Int->[Int] -> [Int]
genFirst2 _ [x] = []
genFirst2 idx (x:xs) = let new = (idx + x + 1)
                        in new:(genFirst2 new xs)

genNextSol::[Int]->[Int]->Int->Maybe [Int]
genNextSol curSol rowGr rowSize =  if (last curSol) + (last rowGr) < rowSize then
                                        Just (incrementLast curSol)
                                   else
                                        let dropFrom = calcDropFrom curSol rowGr rowSize
                                        in (genNextSol2 dropFrom curSol rowGr rowSize)

genNextSol2 Nothing _ _ _ = Nothing
genNextSol2 (Just dropFrom) curSol rowGr rowSize =
                                let partSol = take dropFrom curSol
                                    newPartSol = incrementLast partSol
                                    skipedGr = skipFromNth (dropFrom -1) rowGr
                                    restOfSol = (genFirst2 (last newPartSol) skipedGr)
                                    in Just (newPartSol ++ restOfSol)

incrementLast [x] = [x+1]
incrementLast (x:xs) = x:(incrementLast xs)

incrementFirst (x:xs) = (x+1):xs

safeIncrement (Just x) = Just(x + 1)
safeIncrement Nothing = Nothing

safeDiff Nothing _ = Nothing
safeDiff _ Nothing = Nothing
safeDiff (Just x) (Just y) = Just (x - y)

skipFromNth:: Int->[a]->[a]
skipFromNth 0 list = list
skipFromNth n (x:xs) = skipFromNth (n-1) xs

calcDropFrom :: [Int]-> [Int]->Int->(Maybe Int)
calcDropFrom curSol rowGr rowSize = let curSolRev = reverse curSol
                                        rowGrRev = reverse rowGr
                                        dropFromRev = calcDropFrom2 curSolRev rowGrRev rowSize
                                        len = length(curSol)
                                        jLen = (Just len)
                                    in safeDiff jLen dropFromRev

calcDropFrom2 [s] [rg] rowSize = if s + rg > rowSize then Nothing else Just 0
calcDropFrom2 (s:curSolRev) (rg:rowGrRev) rowSize = if s + rg <= rowSize then
                                                            Just 0
                                                       else
                                                            safeIncrement (calcDropFrom2 curSolRev rowGrRev (rowSize - rg - 1))


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


