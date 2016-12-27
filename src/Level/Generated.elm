module Level.Generated exposing (levels)

import Level.Model exposing (..)

levels : List (Int, Level)
levels =
  ((0,{ size = 9, player = { x = 7, y = 8 }, food = [{ x = 8, y = 8 }], walls = [] })) ::
  ((1,{ size = 9, player = { x = 0, y = 2 }, food = [{ x = 1, y = 3 }], walls = [] })) ::
  ((2,{ size = 9, player = { x = 4, y = 5 }, food = [{ x = 4, y = 4 },{ x = 3, y = 3 }], walls = [] })) ::
  ((3,{ size = 9, player = { x = 1, y = 3 }, food = [{ x = 1, y = 1 },{ x = 2, y = 3 }], walls = [] })) ::
  ((4,{ size = 9, player = { x = 2, y = 4 }, food = [{ x = 2, y = 3 },{ x = 0, y = 1 },{ x = 1, y = 2 }], walls = [] })) ::
  []
