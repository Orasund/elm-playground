module Main exposing (..)

import Browser
import Fish exposing (Fish)
import Random exposing (Seed)
import Rule exposing (Pattern(..))
import View


type alias Model =
    { seed : Seed
    , fish : List Fish
    , animationFrame : Bool
    }


type Msg
    = NextAnimationRequested


init : () -> ( Model, Cmd Msg )
init () =
    let
        times l1 l2 =
            l1
                |> List.concatMap
                    (\a ->
                        l2 |> List.map (\b -> [ a, b ])
                    )

        r a =
            [ [ Horizontal ]
            , [ Vertical ]
            , [ TopDown ]
            , [ BottomUp ]
            , [ Horizontal, Vertical ]
            , [ Horizontal, TopDown ]
            , [ Horizontal, BottomUp ]
            , [ Vertical, TopDown ]
            , [ Vertical, BottomUp ]
            , [ TopDown, BottomUp ]
            , [ Horizontal, Vertical, TopDown ]
            , [ Horizontal, Vertical, BottomUp ]
            , [ Horizontal, TopDown, BottomUp ]
            , [ Vertical, TopDown, BottomUp ]
            , [ Horizontal, Vertical, TopDown, BottomUp ]
            ]
                |> List.map (List.map (Tuple.pair a))

        generators =
            times
                (r True)
                (r False)
                |> List.map List.concat

        --}
        ( patterns, seed ) =
            generators
                |> List.foldl
                    (\rules ->
                        Random.andThen
                            (\l ->
                                rules
                                    |> Fish.generate
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
