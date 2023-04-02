module Main exposing (..)

import Browser
import Fish
import FishPattern
import Html exposing (Html)
import Random exposing (Seed)
import Time
import View


type alias Model =
    { seed : Seed
    , fish : List (List ( Int, Int ))
    , animationFrame : Bool
    }


type Msg
    = NextAnimationRequested


init : () -> ( Model, Cmd Msg )
init () =
    let
        generators =
            [ []
            , FishPattern.horizontal
            , FishPattern.vertical
            , FishPattern.diagonal1
            , FishPattern.diagonal2
            , FishPattern.horizontal ++ FishPattern.vertical
            , FishPattern.horizontal ++ FishPattern.diagonal1
            , FishPattern.horizontal ++ FishPattern.diagonal2
            , FishPattern.vertical ++ FishPattern.diagonal1
            , FishPattern.vertical ++ FishPattern.diagonal2
            , FishPattern.diagonal1 ++ FishPattern.diagonal2
            , FishPattern.horizontal ++ FishPattern.vertical ++ FishPattern.diagonal1
            , FishPattern.horizontal ++ FishPattern.vertical ++ FishPattern.diagonal2
            , FishPattern.horizontal ++ FishPattern.diagonal1 ++ FishPattern.diagonal2
            , FishPattern.vertical ++ FishPattern.diagonal1 ++ FishPattern.diagonal2
            , FishPattern.horizontal ++ FishPattern.vertical ++ FishPattern.diagonal1 ++ FishPattern.diagonal2
            ]

        ( patterns, seed ) =
            generators
                |> List.foldl
                    (\rules ->
                        Random.andThen
                            (\l ->
                                Fish.generatePattern rules
                                    |> Random.map (\p -> p :: l)
                            )
                    )
                    (Random.constant [])
                |> (\gen -> Random.step gen (Random.initialSeed 42))
    in
    ( { seed = seed
      , fish = patterns
      , animationFrame = False
      }
    , Cmd.none
    )


view : Model -> Browser.Document msg
view model =
    { title = "Fish Tank"
    , body =
        [ View.tank
            { animationFrame = model.animationFrame
            }
            model.fish
        ]
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NextAnimationRequested ->
            ( { model | animationFrame = not model.animationFrame }
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



--Time.every 500 (\_ -> NextAnimationRequested)


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
