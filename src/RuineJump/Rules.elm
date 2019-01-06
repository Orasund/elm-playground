module RuineJump.Rules exposing (parkour, placeDirt, placeGrass)

import CellAutomata exposing (Rule, RuleExpression(..))
import RuineJump.MapElement exposing (Block(..))


parkour : List (Rule Block)
parkour =
    [ { from = Nothing
      , neighbors =
            { north = Anything
            , northEast = Exactly <| Nothing
            , east = Exactly <| Just Stone
            , southEast = Exactly <| Nothing
            , south = Anything
            , southWest = Anything
            , west = Anything
            , northWest = Anything
            }
      , to = Just Stone
      }
    ]


placeGrass : List (Rule Block)
placeGrass =
    [ { from = Just Dirt
      , neighbors =
            { north = Exactly <| Nothing
            , northEast = Anything
            , east = Anything
            , southEast = Anything
            , south = Anything
            , southWest = Anything
            , west = Anything
            , northWest = Anything
            }
      , to = Just Grass
      }
    , { from = Just Dirt
      , neighbors =
            { north = Exactly <| Just Grass
            , northEast = Anything
            , east = Anything
            , southEast = Anything
            , south = Anything
            , southWest = Anything
            , west = Anything
            , northWest = Exactly <| Just Grass
            }
      , to = Just Grass
      }
    , { from = Just Dirt
      , neighbors =
            { north = Exactly <| Just Grass
            , northEast = Exactly <| Just Grass
            , east = Anything
            , southEast = Anything
            , south = Anything
            , southWest = Anything
            , west = Anything
            , northWest = Anything
            }
      , to = Just Grass
      }
    ]


placeDirt : List (Rule Block)
placeDirt =
    [ { from = Nothing
      , neighbors =
            { north = Anything
            , northEast = Anything
            , east = Exactly <| Just Dirt
            , southEast = Exactly <| Just Dirt
            , south = Exactly <| Just Dirt
            , southWest = Exactly <| Just Dirt
            , west = Exactly <| Just Dirt
            , northWest = Anything
            }
      , to = Nothing
      }
    , { from = Nothing
      , neighbors =
            { north = Anything
            , northEast = Anything
            , east = Anything
            , southEast = Exactly <| Just Dirt
            , south = Exactly <| Just Dirt
            , southWest = Exactly <| Just Dirt
            , west = Anything
            , northWest = Anything
            }
      , to = Just Dirt
      }
    , { from = Just Dirt
      , neighbors =
            { north = Anything
            , northEast = Anything
            , east = Exactly <| Nothing
            , southEast = Exactly <| Just Dirt
            , south = Exactly <| Just Dirt
            , southWest = Exactly <| Just Dirt
            , west = Exactly <| Nothing
            , northWest = Anything
            }
      , to = Nothing
      }
    ]
