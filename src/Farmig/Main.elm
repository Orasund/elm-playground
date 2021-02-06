module Farmig.Main exposing (main)

import Browser
import Browser.Events as Events
import Dict exposing (Dict)
import Element exposing (Element)
import Element.Border as Border
import Element.Font as Font
import Farmig.Data.Cell exposing (Cell)
import Farmig.Data.Game as Game exposing (Game)
import Farmig.Data.Input as Input
import Farmig.Data.Item as Item exposing (Item)
import Farmig.View.Grid as Grid
import Random exposing (Seed)
import Widget
import Widget.Customize as Customize
import Widget.Material as Material


type alias Model =
    { game : Game
    , seed : Seed
    }


type Msg
    = Move (Maybe ( Int, Int ))


init : ( Model, Cmd Msg )
init =
    let
        ( game, seed ) =
            Random.step Game.init (Random.initialSeed 42)
    in
    ( { game = game
      , seed = seed
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Move maybePos ->
            case maybePos of
                Just pos ->
                    if model.game.food > 0 then
                        let
                            ( game, seed ) =
                                Random.step (model.game |> Game.update pos)
                                    model.seed
                        in
                        ( { game = game
                          , seed = seed
                          }
                        , Cmd.none
                        )

                    else
                        ( model, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Events.onKeyDown (Input.decoder Move)


view : Model -> Element Msg
view model =
    Widget.fullBleedItem
        (Material.fullBleedItem Material.defaultPalette
            |> Customize.element [ Element.width Element.fill ]
        )
        { text = "Food: " ++ String.fromInt model.game.food
        , onPress = Nothing
        , icon = always Element.none
        }
        :: (model.game.world
                |> Grid.view
                    { player = model.game.player
                    , screenSize = 7
                    , food = model.game.food
                    , item = model.game.item
                    }
           )
        ++ (model.game.item
                |> Maybe.map
                    (\item ->
                        [ Widget.multiLineItem
                            (Material.multiLineItem Material.defaultPalette
                                |> Customize.element 
                                    [ Element.width Element.fill
                                    , Font.family [Font.serif]
                                    ]
                            )
                            { title = item  |> Item.name
                            , text = item  |> Item.description
                            , onPress = Nothing
                            , icon =
                                \{ size } ->
                                    item
                                        |> Item.toString
                                        |> Element.text
                                        |> Element.el
                                            [ Element.width <| Element.px size
                                            , Element.height <| Element.px size
                                            , Font.size size
                                            , Font.family
                                                [ Font.external
                                                    { url = "font.css"
                                                    , name = "Noto Emoji"
                                                    }
                                                ]
                                            ]
                            , content = always Element.none
                            }
                        ]
                    )
                |> Maybe.withDefault 
                    [ Widget.multiLineItem
                            (Material.multiLineItem Material.defaultPalette
                                |> Customize.element 
                                    [ Element.width Element.fill
                                    , Font.family [Font.serif]
                                    ]
                            )
                            { title = "No item picked up"
                            , text = "Items have a colored background."
                            , onPress = Nothing
                            , icon =
                                always Element.none
                            , content = always Element.none
                            }
                            ]
           )
        |> Widget.itemList
            (Material.cardColumn Material.defaultPalette
                |> Customize.elementColumn
                    [ Element.width <| Element.px 358
                    , Element.centerX
                    , Element.centerY
                    ]
                |> Customize.mapContent
                    (Customize.element
                        [ Element.padding 0
                        , Element.width <| Element.shrink
                        ]
                        >> Customize.ifFirst
                            [ Border.widthEach
                                { bottom = 1
                                , left = 1
                                , right = 1
                                , top = 1
                                }
                            ]
                        >> Customize.otherwise
                            [ Border.widthEach
                                { bottom = 1
                                , left = 1
                                , right = 1
                                , top = 0
                                }
                            ]
                    )
            )


main : Program () Model Msg
main =
    Browser.element
        { init = always init
        , view =
            view
                >> Element.layout
                    [ Font.family
                        [ Font.external
                            { url = "font.css"
                            , name = "Noto Emoji"
                            }
                        ]
                    ]
        , update = update
        , subscriptions = subscriptions
        }
