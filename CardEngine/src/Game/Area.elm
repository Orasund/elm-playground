module Game.Area exposing (..)

import Game.Card
import Game.Stack exposing (StackItem)
import Html exposing (Attribute, Html)
import Html.Attributes
import Html.Events
import Html.Keyed


type alias Entity msg =
    { content : List (Attribute msg) -> Html msg
    , rotation : Float
    , movement : ( Float, Float )
    }


fromHtml : ( Float, Float ) -> String -> (List (Attribute msg) -> Html msg) -> ( String, Entity msg )
fromHtml offset id content =
    Game.Stack.item content
        |> fromStackItem offset
            (\fun ->
                ( id, \attrs -> fun attrs )
            )


fromStackItem : ( Float, Float ) -> (a -> ( String, List (Attribute msg) -> Html msg )) -> StackItem a -> ( String, Entity msg )
fromStackItem ( x, y ) fun stackItem =
    fun stackItem.card
        |> (\( id, content ) ->
                ( id
                , { content = content
                  , movement = stackItem.movement |> Tuple.mapBoth ((+) x) ((+) y)
                  , rotation = stackItem.rotation
                  }
                )
           )


toHtml : List (Attribute msg) -> List ( String, Entity msg ) -> Html msg
toHtml attr list =
    list
        |> List.sortBy Tuple.first
        |> List.map
            (Tuple.mapSecond
                (\entity ->
                    entity.content
                        [ Html.Attributes.style "position" "absolute"
                        , Html.Attributes.style "transition" "transform 0.5s"
                        , Game.Card.transform
                            [ Game.Card.move entity.movement
                            , Game.Card.rotate entity.rotation
                            ]
                        ]
                )
            )
        |> Html.Keyed.node "div"
            ([ Html.Attributes.style "display" "flex"
             , Html.Attributes.style "position" "relative"
             ]
                ++ attr
            )


hoverable : { onEnter : Maybe msg, onLeave : Maybe msg } -> List (Attribute msg)
hoverable args =
    [ args.onEnter |> Maybe.map Html.Events.onMouseEnter
    , args.onLeave |> Maybe.map Html.Events.onMouseLeave
    ]
        |> List.filterMap identity


{-| assigns three events: onMouseUp, onMouseDown and onClick (useful for touch screens).

onClick will perform the onPress action and if that does not exist, it will perform the onRelease action instead.

-}
draggable : { onPress : Maybe msg, onRelease : Maybe msg } -> List (Attribute msg)
draggable args =
    [ Html.Attributes.style "user-select" "none" |> Just
    , args.onRelease |> Maybe.map Html.Events.onMouseUp
    , args.onPress |> Maybe.map Html.Events.onMouseDown
    , (case args.onPress of
        Just a ->
            Just a

        Nothing ->
            args.onRelease
      )
        |> Maybe.map Html.Events.onClick
    ]
        |> List.filterMap identity
