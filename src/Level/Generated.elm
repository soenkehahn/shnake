module Level.Generated exposing (levels)

import Level.Model exposing (..)

levels : List (Int, Level)
levels =
  ((0,{ size = 9, player = { x = 8, y = 0 }, food = [{ x = 7, y = 0 }], walls = [] })) ::
  ((1,{ size = 9, player = { x = 8, y = 0 }, food = [{ x = 6, y = 1 }], walls = [] })) ::
  ((2,{ size = 9, player = { x = 8, y = 2 }, food = [{ x = 7, y = 3 }], walls = [] })) ::
  ((3,{ size = 9, player = { x = 8, y = 0 }, food = [{ x = 7, y = 1 }], walls = [] })) ::
  ((4,{ size = 9, player = { x = 8, y = 0 }, food = [{ x = 7, y = 1 },{ x = 8, y = 3 }], walls = [] })) ::
  ((5,{ size = 9, player = { x = 5, y = 2 }, food = [{ x = 5, y = 4 },{ x = 4, y = 3 }], walls = [] })) ::
  ((6,{ size = 9, player = { x = 0, y = 2 }, food = [{ x = 5, y = 6 },{ x = 4, y = 6 }], walls = [] })) ::
  []
