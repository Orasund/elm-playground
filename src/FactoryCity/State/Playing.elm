module FactoryCity.State.Playing exposing (Model, Msg, TransitionData, init, subscriptions, update, view)

import Action
import Bag exposing (Bag)
import Element exposing (Element)
import Element.Font as Font
import FactoryCity.Data as Data
import FactoryCity.Data.CellType exposing (ContainerSort(..), RemovableSort(..))
import FactoryCity.Data.Game as Game exposing (Game, Tab(..))
import FactoryCity.Data.Item as Item exposing (Item(..))
import FactoryCity.Data.RemoteShop as RemoteShop
import FactoryCity.View.Game as Game
import Framework.Color as Color
import Framework.Grid as Grid
import Framework.Heading as Heading
import Grid.Bordered as Grid
import Grid.Direction exposing (Direction(..))
import Grid.Position exposing (Position)
import Http exposing (Error(..))
import Maybe.Extra as Maybe
import Random exposing (Seed)
import Task
import Time
import View.WrappedColumn as WrappedColumn



----------------------
-- Model
----------------------


type alias Ui =
    { selected : Maybe ContainerSort
    , running : Bool
    , speed : Int
    , displayedTier : Int
    , wrappedColumn : WrappedColumn.Model Game.Tab
    }


type alias Model =
    { game : Game
    , ui : Ui
    , seed : Seed
    }


type UiMsg
    = ClickedChangeSpeed Int
    | ClickedTierTab Int
    | Selected (Maybe ContainerSort)
    | ClickedViewInfo ContainerSort
    | WrappedColumnSpecific (WrappedColumn.Msg Game.Tab)


type Msg
    = UiSpecific UiMsg
    | GameSpecific Game.Msg
    | PositionSelected Position
    | TimePassed
    | ClickedCraft ContainerSort


type alias TransitionData =
    { shop : Bag String
    , seed : Seed
    , source : Item
    }


type alias Action =
    Action.Action Model Msg Never ()



----------------------
-- Init
----------------------


init : TransitionData -> ( Model, Cmd Msg )
init { shop, seed, source } =
    let
        ( wrappedColumn, cmd ) =
            WrappedColumn.init
                { labels = Game.tabToLabel
                , arrangement = Game.arrangement
                }
    in
    ( { game = Game.init source shop
      , ui =
            { selected = Nothing
            , running = False
            , speed = 1
            , wrappedColumn = wrappedColumn
            , displayedTier = 0
            }
      , seed = seed
      }
    , Cmd.batch
        [ Item.itemList
            |> List.map (\i -> RemoteShop.remove i 5)
            |> Task.sequence
            |> Task.andThen
                (\_ ->
                    RemoteShop.sync
                )
            |> Task.attempt (Game.GotShopResponse >> GameSpecific)
        , cmd
            |> Cmd.map (WrappedColumnSpecific >> UiSpecific)
        ]
    )



----------------------
-- Update
----------------------


updateUi : UiMsg -> Ui -> ( Ui, Cmd UiMsg )
updateUi msg ui =
    case msg of
        ClickedChangeSpeed n ->
            ( { ui | speed = n }
            , Cmd.none
            )

        Selected maybeSelect ->
            ( { ui
                | selected =
                    maybeSelect
                        |> Maybe.andThen
                            (\select ->
                                if ui.selected == Just select then
                                    Nothing

                                else
                                    Just select
                            )
              }
            , Cmd.none
            )

        ClickedTierTab n ->
            ( { ui | displayedTier = n }
            , Cmd.none
            )

        ClickedViewInfo card ->
            ( { ui | selected = Just <| card }
            , ui.wrappedColumn
                |> WrappedColumn.jumpTo DetailsTab
                |> Cmd.map WrappedColumnSpecific
            )

        WrappedColumnSpecific specificMsg ->
            ui.wrappedColumn
                |> WrappedColumn.update specificMsg
                |> Tuple.mapBoth
                    (\wrappedColumn ->
                        { ui
                            | wrappedColumn = wrappedColumn
                        }
                    )
                    (Cmd.map WrappedColumnSpecific)


update : Msg -> Model -> Action
update msg ({ ui, game, seed } as model) =
    case msg of
        TimePassed ->
            seed
                |> Random.step (Game.tick game)
                |> (\( ( g, cmd ), s ) ->
                        ( { model | game = g, seed = s }
                        , cmd |> Cmd.map GameSpecific
                        )
                   )
                |> Action.updating

        PositionSelected position ->
            updateUi (Selected <| Nothing) ui
                |> (\( u, cmd ) ->
                        ( { model
                            | game =
                                game
                                    |> (case ui.selected of
                                            Just cellType ->
                                                Game.playCard cellType position

                                            Nothing ->
                                                Game.removeCard position
                                       )
                            , ui = u
                          }
                        , cmd |> Cmd.map UiSpecific
                        )
                   )
                |> Action.updating

        ClickedCraft card ->
            updateUi (Selected <| Just card) ui
                |> (\( u, cmd ) ->
                        ( { model
                            | game = game |> Game.craft card
                            , ui = u
                          }
                        , Cmd.batch
                            [ cmd
                            , ui.wrappedColumn
                                |> WrappedColumn.jumpTo GameTab
                                |> Cmd.map WrappedColumnSpecific
                            ]
                            |> Cmd.map UiSpecific
                        )
                   )
                |> Action.updating

        UiSpecific uiMsg ->
            ui
                |> updateUi uiMsg
                |> (\( u, cmd ) ->
                        Action.updating
                            ( { model | ui = u }
                            , cmd |> Cmd.map UiSpecific
                            )
                   )

        GameSpecific gameMsg ->
            game
                |> Game.update gameMsg
                |> (\( g, cmd ) ->
                        Action.updating
                            ( { model | game = g }
                            , cmd |> Cmd.map GameSpecific
                            )
                   )


subscriptions : Model -> Sub Msg
subscriptions { ui } =
    if ui.speed /= 0 then
        Sub.batch
            [ Time.every (toFloat <| 1000 // ui.speed) (always TimePassed)
            , Time.every 10000 (always (GameSpecific Game.Sync))
            , WrappedColumn.subscriptions ui.wrappedColumn
                |> Sub.map (WrappedColumnSpecific >> UiSpecific)
            ]

    else
        Sub.none



----------------------
-- View
----------------------


view :
    { scale : Float }
    -> (Msg -> msg)
    -> Model
    -> ( Maybe ( Element msg, Element msg ), List (Element msg) )
view { scale } msgMapper { ui, game } =
    ( Just <|
        ( Element.row
            (Grid.simple
                ++ Color.light
                ++ [ Element.height <| Element.px <| Data.yOffset ]
            )
          <|
            [ Element.el
                [ Element.width <| Element.fill
                , Element.alignBottom
                , Font.alignRight
                ]
              <|
                Element.text <|
                    String.fromInt game.money
                        ++ " Money"
            , Element.el
                (Heading.h1
                    ++ [ Element.width <| Element.fill
                       , Element.alignBottom
                       ]
                )
              <|
                Element.el [ Element.centerX ] <|
                    Element.text <|
                        String.fromInt <|
                            game.stepCount
            , Element.paragraph
                [ Element.width <| Element.fill
                , Element.alignBottom
                ]
              <|
                List.singleton <|
                    Element.text <|
                        "Next bug in "
                            ++ (String.fromInt <| game.nextBugIn)
                            ++ " turns"
            ]
        , ui.wrappedColumn
            |> WrappedColumn.viewButtonRow
            |> Element.map (msgMapper << UiSpecific << WrappedColumnSpecific)
        )
    , [ Element.el [ Element.height <| Element.px <| Data.yOffset ] <| Element.none
      , ui.wrappedColumn
            |> WrappedColumn.view
                (Game.view
                    { scale = scale
                    , selected = ui.selected
                    , positionSelectedMsg = msgMapper << PositionSelected
                    , selectedMsg = Just >> Selected >> UiSpecific >> msgMapper
                    , craftMsg = msgMapper << ClickedCraft
                    , speed = ui.speed
                    , clickedChangeSpeedMsg = ClickedChangeSpeed >> UiSpecific >> msgMapper
                    , clickedTierTabMsg = ClickedTierTab >> UiSpecific >> msgMapper
                    , msgMapper = GameSpecific >> msgMapper
                    , clickedViewInfoMsg = ClickedViewInfo >> UiSpecific >> msgMapper
                    , displayedTier = ui.displayedTier
                    }
                    game
                )
      , Element.el [ Element.height <| Element.px <| 50 ] <| Element.none
      ]
    )
