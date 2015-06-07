import Control.Exception
import System.IO
import System.IO.Error
import Data.List

-- ROZWIĄZYWANIE ŁAMIGŁÓWKI

data Cell = Empty | Black | White | Unknown
                deriving (Eq, Show)

-- utworzenie tablicy pomocniczej
createTable:: Int -> Int-> [[ Cell ]]
createTable colN rowN = [ (createRow colN) | xs <-[1..rowN] ]
createRow colN = [Empty | x<-[1..colN]]

-- główna funkcja rozwiązująca łamigłówkę
solvePuzzle :: ([[Int]], [[Int]]) -> [[Bool]]
solvePuzzle x = convertGame (solvePuzzle2 x)

solvePuzzle2 :: ([[Int]], [[Int]]) -> [[Cell]]
solvePuzzle2 (rows, cols) = let game = createTable (length cols) (length rows)
                                game2 = transpose (solveGameByRow game rows)
                                (solution, transponed) = solveGame game2 cols rows [True | x<-[1..(length cols)]] True
                            in if(transponed) then transpose solution
                               else solution

convertGame :: [[Cell]]-> [[Bool]]
convertGame game = map convertRow game

convertRow :: [Cell]-> [Bool]
convertRow row = map (\x -> x == Black) row

-- kolejne rozwiązania wierszy/kolumn, przyjmowane argumenty:
--   obecne rozwiązanie, dane wierszy, dane kolumn, informacja które kolumny zostały zmienione, informacja czy obrazek jest obrócony
solveGame :: [[Cell]] -> [[Int]] -> [[Int]] -> [Bool] -> Bool -> ([[Cell]], Bool)
solveGame game _ _ [] transponed = (game, transponed)
solveGame game rows cols changedRows transponed =
    if((elemIndex True changedRows) == Nothing) then (game, transponed)
    else
        let analyzedRows = getElem rows changedRows
            analyzedGame = getElem game changedRows
            solution = solveGameByRow analyzedGame analyzedRows
            solRows = merge solution game changedRows
            trGame = transpose solRows
        in solveGame trGame cols rows (findChangedCols analyzedGame solution) (not transponed)

-- wybór elementów listy na podstwie listy z wartościami typu Bool
getElem :: [[a]] -> [Bool] -> [[a]]
getElem _ [] = []
getElem [] _ = []
getElem (elem:elems) (idx:idxs) = if(idx == True) then elem:(getElem elems idxs)
    else getElem elems idxs

-- łączenie wyniku pojedynczej iteracji z całym rozwiazaniem
merge [] game _ = game
merge (sol:sols) (game:games) (change:changes) =
    if (change == True) then sol:(merge sols games changes)
    else game:(merge (sol:sols) games changes)

-- porównanie dwóch tablic [[Cell]]
findChangedCols :: [[Cell]] -> [[Cell]] -> [Bool]
findChangedCols game1 game2 = sumDiffGameCols (diffGames game1 game2)

-- znajdowanie różnic w tablicach (zmiana == True)
diffGames :: [[Cell]] -> [[Cell]] -> [[Bool]]
diffGames [] _ = []
diffGames _ [] = []
diffGames (g1:gs1) (g2:gs2) = (diff g1 g2):(diffGames gs1 gs2)

diff _ [] = []
diff [] _ = []
diff (x:xs) (y:ys) = (x /= y):(diff xs ys)

sumDiffGameCols :: [[Bool]] -> [Bool]
sumDiffGameCols [] = []
sumDiffGameCols (g:gs) = sumDiffGameCols2 g gs

sumDiffGameCols2 :: [Bool] -> [[Bool]] -> [Bool]
sumDiffGameCols2 game [] = game
sumDiffGameCols2 g1 (g2:g2s) = sumDiffGameCols2 (zipWith (||) g1 g2) g2s

-- wyznaczanie rozwiazania dla podanych wierszy
solveGameByRow:: [[Cell]]->[[Int]]->[[Cell]]
solveGameByRow game rows = zipWith (\x y -> trySolveRow x y) game rows

-- rozwiązanie pojedynczego wiersza, argumenty:
--   stare rozwiązanie wiersza, dane wiersza
trySolveRow game rowGr = let refRow = createRow (length game)
                             firstSov = genFirst rowGr
                             refRow2 = trySolveRow2 game rowGr refRow (Just firstSov)
                             newRow = unknownToEmpty refRow2
                             in newRow

unknownToEmpty [] = []
unknownToEmpty (Unknown:list) = Empty:(unknownToEmpty list)
unknownToEmpty (x:list) = x:(unknownToEmpty list)

-- argumenty:
--   stare rozwiązanie wiersza, dane wiersza, wiersz pomocniczy, nowa kombinacja wiersza
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
-- Solucja jest repezentowana jako tablica indeksów, od których zaczynają się kolejne zamalowane grupy
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


-- WCZYTYWANIE DANYCH Z PLIKU

readData :: IO ([[Int]], [[Int]])
readData = do fname <- getLine
              handle <- openFile fname ReadMode
              rowstr <- hGetLine handle
              colstr <- hGetLine handle
              let cols = (read colstr) :: [[Int]]
                  rows = (read rowstr) :: [[Int]]
              hClose handle
              return (rows, cols)

-- RYSOWANIE WYNIKU

-- główna funkcja rysująca
drawPicture :: [[Bool]] -> Int -> IO ()
drawPicture c n = do putChar '+'
                     drawBorderLine n
                     drawContent c n
                     putChar '+'
                     drawBorderLine n
                     return ()

-- rysowanie ramki
drawBorderLine :: Int -> IO ()
drawBorderLine 0 = do putStrLn "+"
                      return ()

drawBorderLine n = do putChar '-'
                      drawBorderLine (n-1)
                      return ()

-- rysowanie zawartości
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

-- PROGRAM GŁÓWNY

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
                             then putStrLn ("Nie istnieje plik o podanej nazwie.")
                         else if isEOFError e
                             then putStrLn ("Brak pełnej informacji o łamigłówce.")
                         else putStrLn ("Błąd programu.")
