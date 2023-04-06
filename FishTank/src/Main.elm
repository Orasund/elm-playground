module Main exposing (..)

import Array
import Browser
import Fish exposing (Fish)
import Layout
import Random exposing (Generator, Seed)
import Rule exposing (Pattern(..))
import View


type alias Random a =
    Generator a


type alias Model =
    { seed : Seed
    , fish : List Fish
    , animationFrame : Bool
    }


type Msg
    = NextAnimationRequested
    | AddFish


init : () -> ( Model, Cmd Msg )
init () =
    let
        r a =
            [ Horizontal, Vertical, TopDown, BottomUp ]
                |> List.map (Tuple.pair a)

        ( patterns, seed ) =
            [ r True ++ r False
            , r True ++ r False
            ]
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


view : Model -> Browser.Document Msg
view model =
    { title = "Fish Tank"
    , body =
        [ View.tank
            { animationFrame = model.animationFrame
            }
            model.fish
        , Layout.textButton []
            { label = "Add Fish"
            , onPress = Just AddFish
            }
        ]
    }


pickTwo : List a -> Random (Maybe ( a, a ))
pickTwo list =
    let
        arr =
            Array.fromList list
    in
    Random.map2
        (\i1 i2 ->
            Maybe.map2 Tuple.pair
                (Array.get
                    (if i1 > i2 then
                        i2

                     else
                        i2 + 1
                    )
                    arr
                )
                (Array.get i1 arr)
        )
        (Random.int 0 (List.length list - 1))
        (Random.int 0 (List.length list - 2))


apply : Seed -> Random Model -> Model
apply s gen =
    Random.step gen s
        |> (\( m, seed ) -> { m | seed = seed })


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NextAnimationRequested ->
            ( { model | animationFrame = not model.animationFrame }
            , Cmd.none
            )

        AddFish ->
            model.fish
                |> pickTwo
                |> Random.andThen
                    (\maybe ->
                        maybe
                            |> Maybe.map
                                (\( fish1, fish2 ) ->
                                    Fish.fromParents fish1 fish2
                                        |> Random.map Just
                                )
                            |> Maybe.withDefault (Random.constant Nothing)
                    )
                |> Random.map
                    (\maybe ->
                        { model
                            | fish =
                                maybe
                                    |> Maybe.map
                                        (\fish ->
                                            fish :: model.fish
                                        )
                                    |> Maybe.withDefault model.fish
                        }
                    )
                |> apply model.seed
                |> (\m -> ( m, Cmd.none ))


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
