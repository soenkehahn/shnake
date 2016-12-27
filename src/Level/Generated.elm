module Level.Generated exposing (levels)

import Level.Model exposing (..)

levels : List (Int, Level)
levels =
  ((0,{ size = 9, player = { x = 8, y = 4 }, food = [{ x = 7, y = 4 }], walls = [] })) ::
  ((1,{ size = 9, player = { x = 8, y = 3 }, food = [{ x = 6, y = 4 }], walls = [] })) ::
  ((2,{ size = 9, player = { x = 4, y = 5 }, food = [{ x = 3, y = 6 }], walls = [] })) ::
  ((3,{ size = 9, player = { x = 8, y = 3 }, food = [{ x = 7, y = 4 }], walls = [] })) ::
  ((4,{ size = 9, player = { x = 8, y = 1 }, food = [{ x = 7, y = 1 },{ x = 8, y = 4 }], walls = [] })) ::
  ((5,{ size = 9, player = { x = 8, y = 2 }, food = [{ x = 7, y = 2 },{ x = 8, y = 4 },{ x = 7, y = 4 }], walls = [] })) ::
  ((6,{ size = 9, player = { x = 3, y = 0 }, food = [{ x = 8, y = 4 }], walls = [] })) ::
  ((7,{ size = 9, player = { x = 0, y = 0 }, food = [], walls = [] })) ::
  ((8,{ size = 9, player = { x = 7, y = 8 }, food = [{ x = 8, y = 8 }], walls = [] })) ::
  ((9,{ size = 9, player = { x = 1, y = 5 }, food = [{ x = 1, y = 3 }], walls = [] })) ::
  ((10,{ size = 9, player = { x = 3, y = 0 }, food = [{ x = 3, y = 3 }], walls = [] })) ::
  ((11,{ size = 9, player = { x = 3, y = 4 }, food = [{ x = 3, y = 0 }], walls = [] })) ::
  ((12,{ size = 9, player = { x = 1, y = 5 }, food = [{ x = 1, y = 0 }], walls = [] })) ::
  ((13,{ size = 9, player = { x = 1, y = 6 }, food = [{ x = 1, y = 0 }], walls = [] })) ::
  ((14,{ size = 9, player = { x = 7, y = 0 }, food = [{ x = 0, y = 0 }], walls = [] })) ::
  ((15,{ size = 9, player = { x = 7, y = 8 }, food = [{ x = 8, y = 8 }], walls = [] })) ::
  ((16,{ size = 9, player = { x = 0, y = 2 }, food = [{ x = 1, y = 3 }], walls = [] })) ::
  ((17,{ size = 9, player = { x = 4, y = 5 }, food = [{ x = 4, y = 4 },{ x = 3, y = 3 }], walls = [] })) ::
  ((18,{ size = 9, player = { x = 1, y = 3 }, food = [{ x = 1, y = 1 },{ x = 2, y = 3 }], walls = [] })) ::
  ((19,{ size = 9, player = { x = 2, y = 4 }, food = [{ x = 2, y = 3 },{ x = 0, y = 1 },{ x = 1, y = 2 }], walls = [] })) ::
  []
