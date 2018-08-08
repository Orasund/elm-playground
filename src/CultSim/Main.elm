module CultSim.Main exposing (main)

import Css
import CultSim.Person as Person exposing (Person, Position)
import Html.Styled exposing (Html, program)
import PixelEngine.Graphics as Graphics exposing (Area)
import PixelEngine.Graphics.Image as Image exposing (image)
import Process
import Random
import Task
import Time exposing (Time)


type alias Model =
    { seed : Random.Seed
    , hunger : Float
    , people : List Person
    }


type Msg
    = Tick Time
    | NewGame Int


init : ( Maybe Model, Cmd Msg )
init =
    Nothing
        ! [ Random.generate NewGame <| Random.int Random.minInt Random.maxInt ]


newGame : Int -> ( Maybe Model, Cmd Msg )
newGame int =
    let
        ( newPerson, seed ) =
            Random.step Person.generate <| Random.initialSeed int
    in
    Just
        { seed = seed
        , hunger = 0
        , people = [ newPerson ]
        }
        ! [ Task.perform Tick
                (Process.sleep (Time.second * 30)
                    |> Task.andThen (\_ -> Time.now)
                )
          ]


updatePeople : (( List Person, Random.Seed ) -> ( List Person, Random.Seed )) -> Model -> Model
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
                                        |> List.foldl
                                            (\person (( newPeople, newSeed ) as tuple) ->
                                                Person.move person newSeed
                                                    |> Tuple.mapFirst
                                                        (\elem ->
                                                            elem :: newPeople
                                                        )
                                            )
                                            ( [], seed )
                                )
                        )
                        ! [ Cmd.none ]

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
        scale : Int
        scale =
            2

        options =
            { scale = toFloat <| scale
            , width = 800
            , transitionSpeedInSec = 0.2
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
                        |> List.map
                            (\{ position, id } ->
                                let
                                    { x, y } =
                                        position
                                in
                                ( ( 400 + x, 300 + y ), Image.image "walking.png" |> Image.movable id )
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
