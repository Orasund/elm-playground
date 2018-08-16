module CultSim.Main exposing (main)

import Css
import CultSim.Person as Person exposing (Action(..), Person, Position)
import Dict exposing (Dict)
import Html.Styled exposing (Html, program)
import Html.Styled.Events as Events
import PixelEngine.Graphics as Graphics exposing (Area)
import PixelEngine.Graphics.Image as Image exposing (image)
import PixelEngine.Graphics.Tile as Tile
import Process
import Random
import Random.Extra as RandomExtra
import Task
import Time exposing (Time)


type alias Model =
    { seed : Random.Seed
    , hunger : Float
    , faith : Int
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
        , faith = 0
        , people = Dict.singleton newId newPerson
        }
        ! [ tickTask 0
          ]


updatePeople2 : (( Dict String Person, Random.Seed ) -> ( Dict String Person, Random.Seed )) -> Model -> Model
updatePeople2 fun model =
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
        Just oldModel ->
            case msg of
                Tick _ ->
                    let
                        model =
                            let
                                newPeople =
                                    oldModel.people |> Dict.filter (\_ person -> person.action /= Dying)

                                newHunger =
                                    oldModel.hunger + 0.25 * (toFloat <| Dict.size newPeople)
                            in
                            if newHunger >= 1 then
                                let
                                    ( maybeId, newSeed ) =
                                        Random.step (RandomExtra.sample <| Dict.keys newPeople) oldModel.seed
                                in
                                { oldModel
                                    | hunger = newHunger
                                    , seed = newSeed
                                    , people =
                                        newPeople
                                            |> (case maybeId of
                                                    Just id ->
                                                        Dict.update id (Maybe.map (\person -> { person | action = Dying }))

                                                    Nothing ->
                                                        identity
                                               )
                                }
                            else
                                { oldModel
                                    | hunger = newHunger
                                    , people = newPeople
                                }
                    in
                    Just
                        (model.people
                            |> Dict.foldl
                                (\id ({ action,praying_duration } as person) ({ people, seed } as m) ->
                                    case action of
                                        PendingPraying ->
                                            { m | people = people |> Dict.update id (Maybe.map <| always <| Person.pray person) }

                                        Praying int ->
                                            let
                                                { hunger, faith } =
                                                    m
                                            in
                                            if int == 0 then
                                                let
                                                    ( newPerson, newSeed ) =
                                                        Person.move person seed
                                                in
                                                { m
                                                    | people = people |> Dict.update id (Maybe.map <| always <| newPerson)
                                                    , seed = newSeed
                                                    , hunger = hunger - 0.2-0.1*(toFloat praying_duration)
                                                    , faith = faith + 1
                                                }
                                            else
                                                { m
                                                    | people = people |> Dict.update id (Maybe.map <| always <| { person | action = Praying <| int - 1 })
                                                    , hunger = hunger - 0.2-0.1*(toFloat praying_duration)
                                                    , faith = faith + 1
                                                }

                                        Walking ->
                                            let
                                                ( newPerson, newSeed ) =
                                                    Person.move person seed
                                            in
                                            { m
                                                | people = people |> Dict.update id (Maybe.map <| always <| newPerson)
                                                , seed = newSeed
                                            }

                                        Dying ->
                                            { m | hunger = 0 }
                                )
                                model
                            |> (\({ faith, people, seed } as m) ->
                                    if faith >= 2 ^ Dict.size people then
                                        let
                                            ( ( newId, newPerson ), newSeed ) =
                                                Random.step Person.generate seed
                                        in
                                        { m
                                            | faith = faith - 2 ^ Dict.size people
                                            , people = people |> Dict.insert newId newPerson
                                            , seed = newSeed
                                        }
                                    else
                                        m
                               )
                            |> (\({ hunger, seed, people } as m) ->
                                    if hunger < 0 then
                                        { m | hunger = 0 }
                                    else if hunger > 1 then
                                        { m | hunger = 1 }
                                    else
                                        m
                               )
                        )
                        ! [ tickTask (Time.second * 8) ]

                Pray id ->
                    let
                        { people } =
                            oldModel
                    in
                    Just
                        { oldModel
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
            , transitionSpeedInSec = 8
            }
    in
    Graphics.render options
        [ Graphics.imageArea
            { height = 600
            , background = Graphics.colorBackground <| Css.rgb 255 255 255
            }
            (case maybeModel of
                Just ({ people, hunger } as model) ->
                    people
                        |> Dict.toList
                        |> List.map
                            (\( id, { position, action, skin, praying_duration } ) ->
                                let
                                    { x, y } =
                                        position

                                    image =
                                        Image.fromTile (Person.tile action)
                                            (Tile.tileset
                                                { source = "bodys/" ++ toString body ++ ".png"
                                                , spriteWidth = 16
                                                , spriteHeight = 16
                                                }
                                            )

                                    { head, body } =
                                        skin
                                in
                                ( ( 400 + x, 300 + y )
                                , Image.multipleImages
                                    (if action == Dying then
                                        [ ( ( 0, 0 ), Image.image "burning.png" ) ]
                                     else
                                        [ ( ( 0, 0 ), image )
                                        , ( case action of
                                                Praying _ ->
                                                    ( 0, 4 )

                                                _ ->
                                                    ( 0, 0 )
                                          , Image.image <| "heads/" ++ toString head ++ ".png"
                                          )
                                        ]
                                            |> (case action of
                                                    Praying int ->
                                                        List.append
                                                            [ ( ( 0, 33 )
                                                              , Image.fromTile
                                                                    (Person.tile_bar
                                                                        (16 // (praying_duration+1)*(int+1)) 
                                                                        (16 // (praying_duration+1)-1)
                                                                        |> Tile.animated 7
                                                                    )
                                                                    (Tile.tileset
                                                                        { source = "blue_bar.png"
                                                                        , spriteWidth = 16
                                                                        , spriteHeight = 4
                                                                        }
                                                                    )
                                                              )
                                                            ]

                                                    _ ->
                                                        identity
                                               )
                                    )
                                    |> Image.movable id
                                    |> (case action of
                                            Walking ->
                                                Image.withAttributes [ Events.onClick (Pray id) ]

                                            _ ->
                                                identity
                                       )
                                )
                            )
                        |> List.append
                            [ ( ( 400 - 64, 300 - 64 )
                              , if hunger < 1 then
                                    Image.image "temple.png"
                                else
                                    Image.image "devil_temple.png"
                              )
                            ]

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
