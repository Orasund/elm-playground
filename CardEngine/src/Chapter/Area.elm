module Chapter.Area exposing (..)

import Dict exposing (Dict)
import ElmBook.Actions
import ElmBook.Chapter exposing (Chapter)
import Game.Card
import Game.Stack
import Html exposing (Html)
import Html.Attributes
import Html.Events
import View.Component


type alias AreaId =
    { areaId : Int }


type alias CardId =
    { cardId : Int }


type alias Card =
    { inArea : AreaId
    }


type alias State =
    { cards : Dict Int Card
    , areas : Dict Int (List CardId)
    , dragging : Maybe CardId
    }


type Msg
    = StartDragging AreaId
    | DraggedOnto AreaId
    | StopDragging


init : State
init =
    { cards =
        List.repeat 5 { inArea = AreaId 0 }
            |> List.indexedMap Tuple.pair
            |> Dict.fromList
    , areas =
        Dict.fromList
            [ ( 0, List.range 0 4 |> List.map CardId ) ]
    , dragging = Nothing
    }


update : Msg -> State -> State
update msg state =
    case msg of
        StartDragging { areaId } ->
            { state
                | dragging =
                    state.areas
                        |> Dict.get areaId
                        |> Maybe.andThen List.head
            }

        StopDragging ->
            { state | dragging = Nothing }

        DraggedOnto id ->
            state.dragging
                |> Maybe.map
                    (\{ cardId } ->
                        { state
                            | dragging = Nothing
                            , cards =
                                state.cards
                                    |> Dict.update cardId
                                        (Maybe.map (\card -> { card | inArea = id }))
                            , areas =
                                state.cards
                                    |> Dict.get cardId
                                    |> Maybe.map
                                        (\card ->
                                            state.areas
                                                |> Dict.update card.inArea.areaId
                                                    (\maybe ->
                                                        maybe
                                                            |> Maybe.map (List.filter ((/=) (CardId cardId)))
                                                    )
                                                |> Dict.update id.areaId
                                                    (\maybe ->
                                                        maybe
                                                            |> Maybe.withDefault []
                                                            |> (::) (CardId cardId)
                                                            |> Just
                                                    )
                                        )
                                    |> Maybe.withDefault state.areas
                        }
                    )
                |> Maybe.withDefault state


draggable : State -> Html Msg
draggable state =
    let
        draggedFromArea =
            state.dragging
                |> Maybe.andThen (\{ cardId } -> state.cards |> Dict.get cardId)
                |> Maybe.map .inArea
    in
    List.repeat 2 ()
        |> List.indexedMap (\i () -> state.areas |> Dict.get i |> Maybe.withDefault [])
        |> List.indexedMap
            (\i list ->
                list
                    |> List.filterMap
                        (\cardId ->
                            state.cards
                                |> Dict.get cardId.cardId
                                |> Maybe.map (Tuple.pair cardId)
                        )
                    |> pile
                        { onStartDragging =
                            if draggedFromArea /= Nothing then
                                Nothing

                            else
                                StartDragging (AreaId i) |> Just
                        , onStopDragging =
                            if draggedFromArea == Just (AreaId i) then
                                StopDragging |> Just

                            else
                                DraggedOnto (AreaId i) |> Just
                        , dragging = state.dragging
                        }
            )
        |> Html.div
            [ Html.Attributes.style "display" "flex"
            , Html.Attributes.style "gap" "8px"
            ]


pile : { onStartDragging : Maybe msg, onStopDragging : Maybe msg, dragging : Maybe CardId } -> List ( CardId, Card ) -> Html msg
pile args list =
    list
        |> List.reverse
        |> List.map
            (\( cardId, card ) ->
                Game.Stack.item
                    { beingDragged = Just cardId == args.dragging
                    , cardId = cardId
                    , card = card
                    }
            )
        |> List.indexedMap
            (\i stackItem ->
                { stackItem
                    | movement =
                        stackItem.movement
                            |> Tuple.mapBoth
                                ((+) 0)
                                ((+) (-4 * toFloat i))
                    , rotation =
                        if stackItem.card.beingDragged then
                            pi / 16

                        else
                            stackItem.rotation
                }
            )
        |> Game.Stack.toHtml
            ([ args.onStopDragging
                |> Maybe.map Html.Events.onMouseUp
             , args.onStartDragging
                |> Maybe.map Html.Events.onMouseDown
             , (case args.onStartDragging of
                    Just a ->
                        Just a

                    Nothing ->
                        args.onStopDragging
               )
                |> Maybe.map Html.Events.onClick
             ]
                |> List.filterMap identity
            )
            { view =
                \card attrs ->
                    View.Component.defaultCard
                        (Html.Attributes.style "user-select" "none"
                            :: (if card.beingDragged then
                                    [ Html.Attributes.style "z-index" "2" ]

                                else
                                    []
                               )
                            ++ attrs
                        )
            , empty = Game.Card.empty [] "Empty Pile"
            }


chapter : { get : model -> State, setTo : model -> State -> model } -> Chapter model
chapter args =
    ElmBook.Chapter.chapter "Areas"
        |> ElmBook.Chapter.withChapterInit
            (\state ->
                ( args.setTo state init, Cmd.none )
            )
        |> ElmBook.Chapter.renderStatefulComponentList
            [ ( "Area"
              , \m ->
                    m
                        |> args.get
                        |> (\state ->
                                draggable state
                                    |> Html.map
                                        (\msg ->
                                            ElmBook.Actions.updateState
                                                (\model ->
                                                    model
                                                        |> args.get
                                                        |> update msg
                                                        |> args.setTo model
                                                )
                                        )
                           )
              )
            ]
