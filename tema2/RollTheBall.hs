{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE EmptyDataDecls, MultiParamTypeClasses,
             TypeSynonymInstances, FlexibleInstances,
             InstanceSigs #-}
module RollTheBall where

import Pipes
import ProblemState

import qualified Data.Array as A
import Data.Maybe

{-
    Direcțiile în care se poate mișca o piesa pe tablă
-}

data Directions = North | South | West | East
    deriving (Eq, Ord, Show)

{-
    Sinonim tip de date pentru reprezetarea unei perechi (Int, Int)
    care va reține coordonatele celulelor de pe tabla de joc
-}

type Position = (Int, Int)

{-
    Tip de date pentru reprezentarea celulelor tablei de joc
-}

data Cell = EmptyCell | HorPipe | VerPipe | TopLeft | BotLeft | BotRight | TopRight | EmptySpace | StartDown | StartLeft | StartRight | StartUp | WinUp | WinDown | WinLeft | WinRight deriving (Eq, Ord)

instance Show Cell where
    show cell = case cell of
        EmptyCell -> [emptyCell]
        HorPipe -> [horPipe]
        VerPipe -> [verPipe]
        TopLeft -> [topLeft]
        BotLeft -> [botLeft]
        BotRight -> [botRight]
        TopRight -> [topRight]
        EmptySpace -> [emptySpace]
        StartUp -> [startUp]
        StartDown -> [startDown]
        StartLeft -> [startLeft]
        StartRight -> [startRight]
        WinUp -> [winUp]
        WinDown -> [winDown]
        WinLeft -> [winLeft]
        WinRight -> [winRight]

{-
    Tip de date pentru reprezentarea nivelului curent
-}

data Level = EmptyLevel | Lv {lvlCells :: (A.Array Position Cell)} deriving (Eq, Ord)
{-
    *** Optional *** 
  
    Dacă aveți nevoie de o funcționalitate particulară,
    instantiați explicit clasele Eq și Ord pentru Level.
    În cazul acesta, eliminați deriving (Eq, Ord) din Level.
-}

{-
    *** TODO ***

    Instanțiati Level pe Show. 
    Atenție! Fiecare linie este urmată de \n (endl in Pipes).
-}

cellToString :: (A.Array Position Cell) -> Int -> Int -> [String]
cellToString cells i m = [show (getCell y) | y <- [0..m]] where
    getCell y = cells A.! (i, y) 

instance Show Level where
    show EmptyLevel = "\n"
    show (Lv cells) = "\n" ++ unlines [concat (cellToString cells i m) | i <- [0..n]] where
        (_, (n, m)) = A.bounds cells

{-  
    *** TODO ***
    Primește coordonatele colțului din dreapta jos a hărții.
    Intoarce un obiect de tip Level în care tabla este populată
    cu EmptySpace. Implicit, colțul din stânga sus este (0,0)
-}

emptyLevel :: Position -> Level
emptyLevel (pos1, pos2) = Lv initCells where
    initCells = A.array ((0,0), (pos1, pos2)) $ [((i, j), EmptySpace) | i <- [0..pos1], j <- [0..pos2]]

{-
    *** TODO ***

    Adaugă o celulă de tip Pipe în nivelul curent.
    Parametrul char descrie tipul de tile adăugat: 
        verPipe -> pipe vertical
        horPipe -> pipe orizontal
        topLeft, botLeft, topRight, botRight -> pipe de tip colt
        startUp, startDown, startLeft, startRight -> pipe de tip initial
        winUp, winDown, winLeft, winRight -> pipe de tip final
    Parametrul Position reprezintă poziția de pe hartă la care va fi adaugată
    celula, dacă aceasta este liberă (emptySpace).
-}
addType :: Char -> Position -> (A.Array Position Cell) -> (A.Array Position Cell)
addType tip pos cells = if(cells A.! pos == EmptySpace) then cells A.//[(pos, newCell)]
    else cells
    where
        newCell 
            |tip == verPipe = VerPipe
            |tip == horPipe = HorPipe
            |tip ==topLeft = TopLeft
            |tip == topRight = TopRight
            |tip == botLeft = BotLeft
            |tip == botRight = BotRight
            |tip == startUp = StartUp
            |tip == startDown = StartDown
            |tip == startRight = StartRight
            |tip == startLeft = StartLeft
            |tip == winUp = WinUp 
            |tip == winDown = WinDown
            |tip == winRight = WinRight
            |tip == winLeft = WinLeft
            |tip == emptyCell = EmptyCell

addCell :: (Char, Position) -> Level -> Level
addCell (tip, pos@(pos1, pos2)) (Lv cells) = if(pos1 < 0 || pos2 < 0 || pos1 > n || pos2 > m) then Lv cells
    else Lv newCells where
        newCells = addType tip pos cells
        (_, (n,m)) = A.bounds cells
{-
    *** TODO *** 

    Primește coordonatele colțului din dreapta jos al hărții și o listă de 
    perechi de tipul (caracter_celulă, poziția_celulei).
    Întoarce un obiect de tip Level cu toate celeule din listă agăugate pe
    hartă.
    Observatie: Lista primită ca parametru trebuie parcursă de la dreapta 
    la stanga.
-} 

createLevel :: Position -> [(Char, Position)] -> Level
createLevel pos s = if(null s) then emptyLevel pos
    else newCells where
    emptyCells = emptyLevel pos
    newCells = foldr addCell emptyCells s

{-
    *** TODO ***

    Mișcarea unei celule în una din cele 4 direcții 
    Schimbul se poate face doar dacă celula vecină e goală (emptySpace).
    Celulele de tip start și win sunt imutabile.

    Hint: Dacă nu se poate face mutarea puteți lăsa nivelul neschimbat.
-}
makeNorth :: Position -> (A.Array Position Cell) -> Level
makeNorth pos@(pos1, pos2) cells = Lv newCells where
    newCells
        | (cells A.! (pos1 - 1, pos2) == EmptySpace && not(cells A.! (pos) == WinLeft) && not(cells A.! (pos) == WinRight) && not(cells A.! (pos) == WinUp) && not(cells A.! (pos) == WinDown)) = cells A.// [((pos1 - 1, pos2), cells A.! (pos)), (pos, EmptySpace)]
 
makeEast :: Position -> (A.Array Position Cell) -> Level
makeEast pos@(pos1, pos2) cells = Lv newCells where
    newCells
        | (cells A.! (pos1, pos2 + 1) == EmptySpace && not(cells A.! (pos) == WinLeft) && not(cells A.! (pos) == WinRight) && not(cells A.! (pos) == WinUp) && not(cells A.! (pos) == WinDown)) = cells A.// [((pos1, pos2 + 1), cells A.! (pos)), (pos, EmptySpace)]

makeSouth :: Position -> (A.Array Position Cell) -> Level
makeSouth pos@(pos1, pos2) cells = Lv newCells where
    newCells
        | (cells A.! (pos1 + 1, pos2) == EmptySpace && not(cells A.! (pos) == WinLeft) && not(cells A.! (pos) == WinRight) && not(cells A.! (pos) == WinUp) && not(cells A.! (pos)== WinDown)) = cells A.// [((pos1 + 1, pos2), cells A.! (pos)), (pos, EmptySpace)]

makeWest :: Position -> (A.Array Position Cell) -> Level
makeWest pos@(pos1, pos2) cells = Lv newCells where
    newCells
        | (cells A.! (pos1, pos2 - 1) == EmptySpace && not(cells A.! (pos) == WinLeft) && not(cells A.! (pos) == WinRight) && not(cells A.! (pos) == WinUp) && not(cells A.! (pos) == WinDown)) = cells A.// [((pos1, pos2 - 1), cells A.! (pos)), (pos, EmptySpace)]

takeCell :: Position -> Directions -> Level -> Level
takeCell pos@(pos1, pos2) dir (Lv cells) = newCells where
    newCells
        | (dir == North) = makeNorth pos cells
        | (dir == East) = makeEast pos cells
        | (dir == West) = makeWest pos cells
        | (dir == South) = makeSouth pos cells
        | otherwise = Lv cells 

moveCell :: Position -> Directions -> Level -> Level
moveCell pos@(pos1, pos2) dir lvl@(Lv cells) = if(pos1 > 0 && pos1 < n && pos2 > 0 && pos2 < m) then newCells
    else Lv cells
    where
        newCells = takeCell pos dir lvl
        (_, (n,m)) = A.bounds cells

{-
    *** HELPER ***

    Verifică dacă două celule se pot conecta.
    Atenție: Direcția indică ce vecin este a
    doua celulă pentru prima.

    ex: connection botLeft horPipe East = True (╚═)
        connection horPipe botLeft East = False (═╚)
-}
connection :: Cell -> Cell -> Directions -> Bool
connection = undefined

{-
    *** TODO ***

    Va returna True dacă jocul este câștigat, False dacă nu.
    Va verifica dacă celulele cu Pipe formează o cale continuă de la celula
    de tip inițial la cea de tip final.
    Este folosită în cadrul Interactive.
-}
wonLevel :: Level -> Bool
wonLevel = undefined

instance ProblemState Level (Position, Directions) where
    successors = undefined
    isGoal = undefined
    reverseAction = undefined
