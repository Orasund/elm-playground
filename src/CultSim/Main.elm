module CultSim.Main exposing (main)

import Css
import CultSim.Person as Person exposing (Action(..), Person, Position)
import Dict exposing (Dict)
import Html.Styled exposing (Html, program)
import Html.Styled.Events as Events
import PixelEngine.Graphics as Graphics exposing (Area)
import PixelEngine.Graphics.Image as Image exposing (image)
import Process
import Random
import Task
import Time exposing (Time)


type alias Model =
    { seed : Random.Seed
    , hunger : Float
    , people : Dict String Person
    }


type Msg
    = Tick Time
    | NewGame Int
    | Pray String


init : ( Maybe Model, Cmd Msg )
init =
    Nothing
        ! [ Random.generate NewGame <| Random.int Random.minInt Random.maxInt ]


tickTask : Time -> Cmd Msg
tickTask delay =
    Task.perform Tick
        (Process.sleep delay
            |> Task.andThen (\_ -> Time.now)
        )


newGame : Int -> ( Maybe Model, Cmd Msg )
newGame int =
    let
        ( ( newId, newPerson ), seed ) =
            Random.step Person.generate <| Random.initialSeed int
    in
    Just
        { seed = seed
        , hunger = 0
        , people = Dict.singleton newId newPerson
        }
        ! [ tickTask (Time.second * 30)
          ]


updatePeople : (( Dict String Person, Random.Seed ) -> ( Dict String Person, Random.Seed )) -> Model -> Model
updatePeople fun model =
    let
        ( people, seed ) =
            fun ( model.people, model.seed )
    in
    { model
        | people = people
        , seed = seed
    }


update : Msg -> Maybe Model -> ( Maybe Model, Cmd Msg )
update msg maybeModel =
    case maybeModel of
        Just model ->
            case msg of
                Tick _ ->
                    Just
                        (model
                            |> updatePeople
                                (\( people, seed ) ->
                                    people
                                        |> Dict.foldl
                                            (\id person (( newPeople, newSeed ) as tuple) ->
                                                Person.update person newSeed
                                                    |> Tuple.mapFirst
                                                        (\elem ->
                                                            newPeople
                                                                |> Dict.update id (always <| Just elem)
                                                        )
                                            )
                                            ( people, seed )
                                )
                        )
                        ! [ tickTask (Time.second * 10) ]

                Pray id ->
                    let
                        { people } =
                            model
                    in
                    Just
                        { model
                            | people = people |> Dict.update id (Maybe.map Person.setPraying)
                        }
                        ! []

                NewGame int ->
                    newGame int

        Nothing ->
            case msg of
                NewGame int ->
                    newGame int

                _ ->
                    Nothing ! [ Cmd.none ]


subscriptions : Maybe Model -> Sub Msg
subscriptions maybeModel =
    Sub.none


view : Maybe Model -> Html Msg
view maybeModel =
    let
        scale : Float
        scale =
            2

        options =
            { scale = scale
            , width = 800
            , transitionSpeedInSec = 10
            }
    in
    Graphics.render options
        [ Graphics.imageArea
            { height = 600
            , background = Graphics.colorBackground <| Css.rgb 255 255 255
            }
            (case maybeModel of
                Just ({ people } as model) ->
                    people
                        |> Dict.toList
                        |> List.map
                            (\( id, { position, action } ) ->
                                let
                                    { x, y } =
                                        position

                                    img_src =
                                        case action of
                                            Walking ->
                                                "walking.png"

                                            PendingPraying ->
                                                "pendingPraying.png"

                                            Praying ->
                                                "praying.png"
                                in
                                ( ( 400 + x, 300 + y )
                                , Image.multipleImages
                                    [ ( ( 0, 0 ), Image.image img_src )
                                    , ( (0,0), Image.image "heads/1.png")
                                    ]
                                    |> Image.movable id
                                    |> Image.withAttributes
                                        [ Events.onClick (Pray id) ]
                                )
                            )

                Nothing ->
                    []
            )
        ]


main : Program Never (Maybe Model) Msg
main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
