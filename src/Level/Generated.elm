module Level.Generated exposing (levels)

import Level.Model exposing (..)

levels : List (Int, Level)
levels =
  ((0,{ size = 9, player = { x = 7, y = 3 }, food = [{ x = 1, y = 8 },{ x = 2, y = 6 },{ x = 8, y = 0 },{ x = 8, y = 7 },{ x = 3, y = 2 },{ x = 5, y = 7 },{ x = 0, y = 4 },{ x = 2, y = 7 },{ x = 2, y = 6 },{ x = 2, y = 3 },{ x = 0, y = 8 }], walls = [{ x = 9, y = 8 },{ x = 9, y = 0 },{ x = 3, y = 0 },{ x = 2, y = 8 },{ x = 5, y = 5 },{ x = 6, y = 3 },{ x = 0, y = 3 },{ x = 8, y = 5 },{ x = 7, y = 1 },{ x = 2, y = 0 },{ x = 3, y = 3 },{ x = 0, y = 0 },{ x = 4, y = 3 }] })) ::
  []
