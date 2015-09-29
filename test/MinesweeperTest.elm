module MinesweeperTest where

import Check exposing (..)
import Check.Test exposing (..)
import Check.Investigator exposing (..)
import Check.Runner.Browser exposing (..)

import Minesweeper

claim_coordinates_add =
  claim
    "Coordinates add as expected."
  `that`
    (\(x, y) -> Minesweeper.addCoord (x, y) (0, 0))
  `is`
    (identity)
  `for`
    tuple (int, int)

suite_list =
  suite "neighbors"
  [ suite "within one"
    [ claim_coordinates_add
    ]
  ]

result = quickCheck suite_list
main = display result
