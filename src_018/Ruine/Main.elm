module Ruine.Main exposing (main)

import Css
import Dict exposing (Dict)
import DigDigBoom.Component.Map as Map
import Html.Styled exposing (Html, program)
import Html.Styled.Events as Events
import PixelEngine.Graphics as Graphics exposing (Area)
import PixelEngine.Graphics.Image as Image exposing (image)
import PixelEngine.Graphics.Tile as Tile exposing (Tileset)
import Random


type alias Model =
    { map : Dict ( Int, Int ) Int
    , seed : Random.Seed
    }


type Msg
    = Init Int


init : Int -> ( Maybe Model, Cmd Msg )
init int =
    ( Just
        { seed = Random.initialSeed int
        , map = Dict.empty
        }
    , Cmd.none
    )


update : Msg -> Maybe Model -> ( Maybe Model, Cmd Msg )
update msg maybeModel =
    case msg of
        Init int ->
            init int


subscriptions : Maybe Model -> Sub Msg
subscriptions maybeModel =
    Sub.none


view : Maybe Model -> Html Msg
view maybeModel =
    let
        scale : Float
        scale =
            2

        width : Float
        width =
            600

        height : Float
        height =
            600

        options =
            { scale = scale
            , width = width
            , transitionSpeedInSec = 8
            }

        rows : Int
        rows =
            32

        tileset : Tileset
        tileset =
            { source = "tileset.png"
            , spriteWidth = 3
            , spriteHeight = 3
            }
    in
    Graphics.render options
        [ Graphics.tiledArea
            { background = Graphics.colorBackground <| Css.rgb 255 255 255
            , rows = rows
            , tileset = tileset
            }
            (case maybeModel of
                Just model ->
                    []

                Nothing ->
                    []
            )
        ]


main : Program Never (Maybe Model) Msg
main =
    program
        { init =
            ( Nothing
            , Random.generate Init <| Random.int Random.minInt Random.maxInt
            )
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
