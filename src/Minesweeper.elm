module Minesweeper where
{-| Inspired by a Minesweeper kata, a first pass at implementing the logic.
-}


-- Utility function for cross-join.
andMap : List (a -> b) -> List a -> List b
andMap fnList range = List.concatMap (\f -> List.map f range) fnList


-- Tuple list of eight neighbor positions for addition.
neighbors =
  List.map (,) [-1..1] `andMap` [-1..1]
  |> List.filter (\x -> x /= (0,0))


-- Add together two coordinate values.
addCoord: ( number, number ) -> ( number, number ) -> ( number, number )
addCoord (x,y) (x',y') =
  (x + x', y + y')


-- All neighbors of a coordinate.
neighborsOf : ( number, number ) -> List ( number, number )
neighborsOf coord =
  List.map (\neighbor -> addCoord neighbor coord) neighbors


-- Predicate to check if a coordinate is within bounds of the board.

-- Assume a square for now to keep is simple.
inBounds min max (x,y) =
  if | x < min -> False
     | y < min -> False
     | x > max -> False
     | y > max -> False
     | otherwise -> True
  

-- Get valid neighbors of a coordinate given a board width.
validNeighbors : number -> ( number, number ) -> List ( number, number )
validNeighbors width coord =
  neighborsOf coord
  |> List.filter (\c -> inBounds 0 (width - 1) c)


-- Get initial grid of coordinates with 0 threats.
makeGrid : number -> List ( ( number, number ), number)
makeGrid width =
  let grid = List.map (,) [0..(width - 1)] `andMap` [0..(width - 1)]
      zeroes = List.repeat (List.length grid) 0
  in
     List.map2 (,) grid zeroes
