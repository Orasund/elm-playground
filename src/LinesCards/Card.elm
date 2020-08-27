module LinesCards.Card exposing (Card(..), Color(..), lines)

import LinesCards.View as View


type Color
    = Y
    | G
    | B
    | R


type Card
    = Cross Color Color
    | VEdges Color Color
    | HEdges Color Color


lines : Color -> List { offset : Float, stroke : Float, color : String }
lines color =
    case color of
        Y ->
            [ { offset = 0
              , stroke = View.relative <| 1
              , color = "black"
              }
            , { offset = View.relative <| 2
              , stroke = View.relative <| 1
              , color = "black"
              }
            , { offset = View.relative <| -1 * 2
              , stroke = View.relative <| 1
              , color = "black"
              }
            , { offset = View.relative <| 5.5
              , stroke = View.relative <| 4
              , color = View.yellow
              }
            , { offset = View.relative <| -1 * 5.5
              , stroke = View.relative <| 4
              , color = View.yellow
              }
            ]

        G ->
            [ { offset = 0
              , stroke = View.relative <| 1
              , color = "black"
              }
            , { offset = View.relative <| 3.5
              , stroke = View.relative <| 4
              , color = View.green
              }
            , { offset = View.relative <| -1 * 3.5
              , stroke = View.relative <| 4
              , color = View.green
              }
            , { offset = View.relative <| 7
              , stroke = View.relative <| 1
              , color = "black"
              }
            , { offset = View.relative <| -1 * 7
              , stroke = View.relative <| 1
              , color = "black"
              }
            ]

        B ->
            [ { offset = 0
              , stroke = View.relative <| 3
              , color = View.blue
              }
            , { offset = View.relative <| 3
              , stroke = View.relative <| 1
              , color = "black"
              }
            , { offset = View.relative <| -3
              , stroke = View.relative <| 1
              , color = "black"
              }
            , { offset = View.relative <| 6
              , stroke = View.relative <| 3
              , color = View.blue
              }
            , { offset = View.relative <| -6
              , stroke = View.relative <| 3
              , color = View.blue
              }
            ]

        R ->
            [ { offset = 0
              , stroke = View.relative <| 7
              , color = View.red
              }
            , { offset = View.relative <| 5
              , stroke = View.relative <| 1
              , color = "black"
              }
            , { offset = View.relative <| -5
              , stroke = View.relative <| 1
              , color = "black"
              }
            , { offset = View.relative <| 7
              , stroke = View.relative <| 1
              , color = "black"
              }
            , { offset = View.relative <| -7
              , stroke = View.relative <| 1
              , color = "black"
              }
            ]
