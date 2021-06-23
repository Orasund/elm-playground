module GreenFields.Page.Lobby exposing (Model, Msg, init, nextSearch, subscriptions, update, view)

import Bag
import Browser
import Color as C
import Dict exposing (Dict)
import Element
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Firestore exposing (Document, Error(..), Firestore)
import GreenFields.Data.Building as Building exposing (Building)
import GreenFields.Data.Database as Database
import GreenFields.Data.Game as Game exposing (Game)
import GreenFields.Data.Resource as Resource
import GreenFields.Data.Tile as Tile exposing (Tile)
import GreenFields.View.Card as Card
import GreenFields.View.Color as Color
import GreenFields.View.Tile as Tile
import Html exposing (Html)
import Http exposing (Error(..))
import Process
import Set exposing (Set)
import Task
import Time exposing (Posix)


type Model
    = Ready Posix
    | Searching
        { searchGraph : Set ( Int, Int )
        , current : ( Int, Int )
        , timestamp : Posix
        }
    | Failed Firestore.Error


type Msg
    = StartSearching
    | GotTime Posix
    | GotResult (Result Firestore.Error (Maybe Tile))
    | GotBoardResult (Result Firestore.Error (Dict ( Int, Int ) Tile))


nextSearch : ( Int, Int ) -> ( Int, Int )
nextSearch ( x, y ) =
    --spirale outwards starting from (0,0)
    if ( x, y ) == ( 0, 0 ) then
        ( 1, 0 )
        {- else if (x + 1 == y) && (0 < x) then
           --bottom right
           ( x, y - 1 )
        -}

    else if (x == -y) && (0 < x) then
        --top right
        ( x - 1, y )

    else if (x == y) && (x < 0) then
        --top left
        ( x, y + 1 )

    else if (x == -y) && (x < 0) then
        --bottom left
        ( x + 1, y )

    else if (abs x > abs y) && (0 < x) then
        --right
        ( x, y - 1 )

    else if (abs y > abs x) && (y < 0) then
        --top
        ( x - 1, y )

    else if (abs x > abs y) && (x < 0) then
        --left
        ( x, y + 1 )

    else
        --bottom
        ( x + 1, y )


init : () -> ( Model, Cmd Msg )
init () =
    ( Ready (Time.millisToPosix 0)
    , Task.perform GotTime Time.now
    )


update :
    { modelMapper : Model -> model
    , upgrade :
        { center : ( Int, Int )
        , timestamp : Posix
        , board : Dict ( Int, Int ) Tile
        }
        -> model
    }
    -> Msg
    -> Model
    -> ( model, Cmd Msg )
update args msg model =
    case model of
        Ready t ->
            case msg of
                StartSearching ->
                    ( Searching
                        { searchGraph = Set.empty
                        , current = ( 0, 0 )
                        , timestamp = t
                        }
                        |> args.modelMapper
                    , Database.getTile ( 0, 0 )
                        |> Task.attempt GotResult
                    )

                GotTime timestamp ->
                    ( Ready timestamp |> args.modelMapper
                    , Cmd.none
                    )

                _ ->
                    ( model |> args.modelMapper, Cmd.none )

        Searching ml ->
            case msg of
                StartSearching ->
                    ( Searching
                        { ml
                            | searchGraph = Set.empty
                            , current = ( 0, 0 )
                        }
                        |> args.modelMapper
                    , Database.getTile ( 0, 0 )
                        |> Task.attempt GotResult
                    )

                GotTime timestamp ->
                    ( Searching
                        { ml | timestamp = timestamp }
                        |> args.modelMapper
                    , Cmd.none
                    )

                GotResult result ->
                    case result of
                        Ok maybeTile ->
                            let
                                current =
                                    ml.current |> nextSearch
                            in
                            case
                                maybeTile
                                    |> Maybe.andThen
                                        (\tile ->
                                            if tile |> Tile.isOld ml.timestamp then
                                                Nothing

                                            else
                                                Just tile
                                        )
                            of
                                Just tile ->
                                    ( Searching
                                        { ml
                                            | searchGraph = ml.searchGraph |> Set.insert ml.current
                                            , current = current
                                        }
                                        |> args.modelMapper
                                    , Database.getTile
                                        (ml.current
                                            |> nextSearch
                                            |> Tuple.mapBoth ((*) Game.distanceBetweenTowns)
                                                ((*) Game.distanceBetweenTowns)
                                        )
                                        |> Task.attempt GotResult
                                    )

                                Nothing ->
                                    ( model |> args.modelMapper
                                    , Database.getBoard
                                        { radius = Game.viewRadius
                                        , position =
                                            ml.current
                                                |> Tuple.mapBoth ((*) Game.distanceBetweenTowns) ((*) Game.distanceBetweenTowns)
                                        }
                                        |> Task.attempt GotBoardResult
                                    )

                        Err err ->
                            ( Failed err |> args.modelMapper, Cmd.none )

                GotBoardResult result ->
                    case result of
                        Ok board ->
                            ( args.upgrade
                                { center = ml.current |> Tuple.mapBoth ((*) Game.distanceBetweenTowns) ((*) Game.distanceBetweenTowns)
                                , board = board
                                , timestamp = ml.timestamp
                                }
                            , Cmd.none
                            )

                        Err err ->
                            ( Failed err |> args.modelMapper, Cmd.none )

        Failed _ ->
            ( model |> args.modelMapper, Cmd.none )


view : Model -> Html Msg
view model =
    let
        radius =
            5
    in
    [ [ " ###  ###   ####  ####  #  #      ####  ###  ####  #     ###    ###"
      , "#     #  #  ##    ##    ## #      ##     #   ##    #     #  #  ##"
      , "#  #  ###   #     #     # ##      #      #   #     #     #  #    ##"
      , " ###  #  #  ####  ####  #  #      #     ###  ####  ####  ###   ###"
      , "==================================================================="
      , " "
      ]
        |> List.map Element.text
        |> Element.column [ Element.centerX ]
    , case model of
        Ready _ ->
            ([ " "
             , "_A Multiplayer game where you cooperatively build a civilization._"
             , " "
             , " "
             , "Features"
             , " "
             , "* Build news towns from the ashes of old civilizations."
             , " "
             , "* Design your city such that future players can extend it."
             , " "
             , "* A game session lasts 10 minutes, so make them count."
             , " "
             , " "
             ]
                |> List.map Element.text
            )
                ++ [ Input.button
                        [ Color.black
                            |> C.toRgba
                            |> Element.fromRgb
                            |> Font.color
                        , Font.bold
                        , Element.centerX
                        ]
                        { onPress = Just StartSearching
                        , label = "[Start New Game]" |> Element.text
                        }
                   ]
                |> Element.column [ Element.centerX ]

        Searching ml ->
            List.range -radius radius
                |> List.map
                    (\y ->
                        List.range -radius radius
                            |> List.map
                                (\x ->
                                    (if ml.searchGraph |> Set.member ( x, y ) then
                                        "X"

                                     else
                                        " "
                                    )
                                        |> Element.text
                                )
                            |> Element.row [ Element.centerX ]
                    )
                |> Element.column [ Element.centerX ]

        Failed err ->
            (case err of
                Http_ e ->
                    case e of
                        BadUrl string ->
                            "Bad Url: " ++ string

                        Timeout ->
                            "Timeout"

                        NetworkError ->
                            "Network Error"

                        BadStatus int ->
                            "Bad Status: " ++ String.fromInt int

                        BadBody string ->
                            "Bad body: " ++ string

                Response e ->
                    "Code "
                        ++ String.fromInt e.code
                        ++ ": "
                        ++ e.message
            )
                |> Element.text
                |> List.singleton
                |> Element.paragraph []
    ]
        |> Element.column
            [ Element.centerY, Element.width Element.fill ]
        |> Element.layout
            [ Color.white
                |> C.toRgba
                |> Element.fromRgb
                |> Font.color
            , Color.green
                |> C.toRgba
                |> Element.fromRgb
                |> Background.color
            , Font.size 14
            , Font.family [ Font.monospace ]
            ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
