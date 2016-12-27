module Level.Generated exposing (levels)

import Level.Model exposing (..)

levels : List (Int, Level)
levels =
  ((0,{ size = 9, player = { x = 0, y = 0 }, food = [], walls = [] })) ::
  ((1,{ size = 9, player = { x = 7, y = 8 }, food = [{ x = 8, y = 8 }], walls = [] })) ::
  ((2,{ size = 9, player = { x = 1, y = 5 }, food = [{ x = 1, y = 3 }], walls = [] })) ::
  ((3,{ size = 9, player = { x = 3, y = 0 }, food = [{ x = 3, y = 3 }], walls = [] })) ::
  ((4,{ size = 9, player = { x = 3, y = 4 }, food = [{ x = 3, y = 0 }], walls = [] })) ::
  ((5,{ size = 9, player = { x = 1, y = 5 }, food = [{ x = 1, y = 0 }], walls = [] })) ::
  ((6,{ size = 9, player = { x = 1, y = 6 }, food = [{ x = 1, y = 0 }], walls = [] })) ::
  ((7,{ size = 9, player = { x = 7, y = 0 }, food = [{ x = 0, y = 0 }], walls = [] })) ::
  []
