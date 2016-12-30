module Level.Generated exposing (levels)

import Level.Model exposing (..)

levels : List (Int, Level)
levels =
  ((0,{ size = 9, player = { x = 4, y = 4 }, food = [{ x = 0, y = 0 }], walls = [{ x = 0, y = 6 },{ x = 2, y = 8 },{ x = 8, y = 2 }] })) ::
  []
