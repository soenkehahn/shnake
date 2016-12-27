module Level.Generated exposing (levels)

import Level.Model exposing (..)

levels : List (Int, Level)
levels =
  ((0,{ size = 5, player = { x = 4, y = 0 }, food = [{ x = 3, y = 0 }], walls = [] })) ::
  ((1,{ size = 3, player = { x = 2, y = 1 }, food = [{ x = 0, y = 2 },{ x = 0, y = 1 },{ x = 0, y = 2 }], walls = [] })) ::
  ((2,{ size = 3, player = { x = 2, y = 1 }, food = [{ x = 1, y = 2 }], walls = [] })) ::
  ((3,{ size = 3, player = { x = 2, y = 1 }, food = [{ x = 1, y = 2 }], walls = [] })) ::
  ((4,{ size = 4, player = { x = 2, y = 0 }, food = [{ x = 2, y = 3 },{ x = 2, y = 3 },{ x = 1, y = 2 }], walls = [] })) ::
  ((5,{ size = 3, player = { x = 2, y = 0 }, food = [{ x = 2, y = 2 },{ x = 2, y = 2 },{ x = 1, y = 1 },{ x = 2, y = 2 },{ x = 1, y = 1 }], walls = [{ x = 0, y = 2 }] })) ::
  ((6,{ size = 5, player = { x = 0, y = 0 }, food = [{ x = 4, y = 4 }], walls = [{ x = 0, y = 1 },{ x = 1, y = 1 },{ x = 2, y = 1 },{ x = 3, y = 3 },{ x = 4, y = 3 }] })) ::
  []
