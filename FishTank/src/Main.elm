module Main exposing (..)

import Browser
import Fish
import Random exposing (Seed)
import Rule
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
            , [ Rule.hor True, Rule.hor False ]
            , [ Rule.ver True, Rule.ver False ]
            , [ Rule.dia1 True, Rule.dia1 False ]
            , [ Rule.dia2 True, Rule.dia2 False ]
            , [ Rule.hor True, Rule.ver False]
            , [ Rule.hor True, Rule.dia1 False]
            , [ Rule.hor True, Rule.dia2 False]
            , [ Rule.ver True, Rule.dia1 False]
            , [ Rule.ver True, Rule.dia2 False]
            , [ Rule.dia1 True, Rule.dia2 False]
            , [ Rule.hor True
            , Rule.ver True
            , Rule.dia1 True
            , Rule.dia2 True
            , Rule.hor False
            , Rule.ver False
            , Rule.dia1 False
            , Rule.dia2 False ]
            ]

        --}
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
subscriptions _ =
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
