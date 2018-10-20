module Ruine.Main exposing (main)

import Css
import Browser exposing (Document)
import PixelEngine exposing (PixelEngine, program)
import Dict exposing (Dict)
import DigDigBoom.Component.Map as Map
import Html.Styled exposing (Html)
import Html.Styled.Events as Events
import PixelEngine.Graphics as Graphics exposing (Area, Options)
import PixelEngine.Graphics.Image as Image exposing (image)
import PixelEngine.Graphics.Tile as Tile exposing (Tileset)
import Random


type alias Model =
    { map : Dict ( Int, Int ) Int
    , seed : Random.Seed
    }


type Msg
    = Init Int
    | None


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
        None ->
            (maybeModel,Cmd.none)


subscriptions : Maybe Model -> Sub Msg
subscriptions maybeModel =
    Sub.none


view : Maybe Model -> { title : String, options : Options Msg, body : List (Area Msg) }
view maybeModel =
    let

        width : Float
        width =
            600

        height : Float
        height =
            600

        options =
            Graphics.options
            { width = width
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
    { title = "Ruine Jump"
    , options = options
    , body = [ Graphics.tiledArea
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
    }
        


main : PixelEngine {} (Maybe Model) Msg
main =
    program
        { init = \_ ->
            ( Nothing
            , Random.generate Init <| Random.int Random.minInt Random.maxInt
            )
        , view = view
        , update = update
        , subscriptions = subscriptions
        , controls = \_ -> None
        }
