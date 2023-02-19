module View.Area exposing (..)

import Game.Area exposing (Entity)
import Game.Stack
import Html exposing (Html)
import Html.Attributes
import View.Component


type alias AreaId =
    { areaId : Int }


type alias CardId =
    { cardId : Int }


hoverable : { onEnter : Int -> msg, onLeave : msg, hoverOver : Maybe Int } -> Html msg
hoverable args =
    List.repeat 3 ()
        |> List.indexedMap
            (\i () ->
                let
                    attrs =
                        Game.Area.hoverable
                            { onEnter = Just (args.onEnter i), onLeave = Just args.onLeave }
                in
                if args.hoverOver == Just i then
                    View.Component.defaultCard attrs

                else
                    View.Component.defaultBack attrs
            )
        |> Html.div
            [ Html.Attributes.style "display" "flex"
            , Html.Attributes.style "flex-direction" "row"
            , Html.Attributes.style "flex-wrap" "wrap"
            , Html.Attributes.style "gap" "8px"
            ]


draggable : { onPress : Int -> Maybe msg, onRelease : Int -> Maybe msg, cardAt : Int, isSelected : Bool } -> Html msg
draggable args =
    List.repeat 3 ()
        |> List.indexedMap
            (\i () ->
                let
                    attrs =
                        Game.Area.draggable
                            { onPress = args.onPress i
                            , onRelease = args.onRelease i
                            }
                in
                (if args.cardAt == i then
                    ()
                        |> Game.Stack.item
                        |> (\stackItem ->
                                if args.isSelected then
                                    { stackItem | rotation = pi / 16 }

                                else
                                    stackItem
                           )
                        |> List.singleton

                 else
                    []
                )
                    |> Game.Area.fromStack ( toFloat i * 150, 0 )
                        { view =
                            \_ () ->
                                ( "draggable__card"
                                , \a -> View.Component.defaultCard (attrs ++ a)
                                )
                        , empty =
                            ( "draggable__empty_" ++ String.fromInt i
                            , \a -> View.Component.empty (attrs ++ a)
                            )
                        }
            )
        |> List.concat
        |> Game.Area.toHtml
            [ Html.Attributes.style "height" "200px"
            ]


pile :
    Int
    ->
        { position : ( Float, Float )
        , onStartDragging : Maybe msg
        , onStopDragging : Maybe msg
        , onEntering : Maybe msg
        , onLeaving : Maybe msg
        }
    -> List { cardId : CardId, card : card, beingDragged : Bool, asPhantom : Bool }
    -> List ( String, Entity msg )
pile index args list =
    let
        attrs =
            Game.Area.draggable { onPress = args.onStartDragging, onRelease = args.onStopDragging }
                ++ Game.Area.hoverable { onEnter = args.onEntering, onLeave = args.onEntering }
    in
    list
        |> List.reverse
        |> List.map Game.Stack.item
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
        |> Game.Area.fromStack args.position
            { view =
                \i card ->
                    ( if card.asPhantom && not card.beingDragged then
                        "pile__phantom"

                      else
                        "pile__" ++ String.fromInt card.cardId.cardId
                    , \a ->
                        View.Component.defaultCard
                            ((if card.beingDragged then
                                [ Html.Attributes.style "z-index" "100" ]

                              else
                                [ Html.Attributes.style "z-index" (String.fromInt (i + 1)) ]
                             )
                                ++ (if card.asPhantom then
                                        [ Html.Attributes.style "filter" "brightness(0.9)"
                                        ]

                                    else
                                        []
                                   )
                                ++ attrs
                                ++ a
                            )
                    )
            , empty = ( "pile__empty__" ++ String.fromInt index, \a -> View.Component.empty (Html.Attributes.style "z-index" "0" :: attrs ++ a) )
            }
