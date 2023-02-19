module Game.Area exposing (..)

import Game.Card
import Game.Pile exposing (PileItem)
import Html exposing (Attribute, Html)
import Html.Attributes
import Html.Events
import Html.Keyed


type alias Entity msg =
    { id : String
    , content : List (Attribute msg) -> Html msg
    , rotation : Float
    , movement : ( Float, Float )
    , zIndex : Int
    }


fromHtml : ( Float, Float ) -> String -> (List (Attribute msg) -> Html msg) -> Entity msg
fromHtml offset id content =
    { id = id
    , content = content
    , movement = offset
    , rotation = 0
    , zIndex = 0
    }


mapRotation : (Float -> Float) -> Entity msg -> Entity msg
mapRotation fun entity =
    { entity | rotation = fun entity.rotation }


mapMovement : (( Float, Float ) -> ( Float, Float )) -> Entity msg -> Entity msg
mapMovement fun entity =
    { entity | movement = fun entity.movement }


mapZIndex : (Int -> Int) -> Entity msg -> Entity msg
mapZIndex fun entity =
    { entity | zIndex = fun entity.zIndex }


fromStack :
    ( Float, Float )
    ->
        { view : Int -> a -> ( String, List (Attribute msg) -> Html msg )
        , empty : ( String, List (Attribute msg) -> Html msg )
        }
    -> List (PileItem a)
    -> List (Entity msg)
fromStack ( x, y ) args list =
    list
        |> List.indexedMap
            (\i stackItem ->
                args.view i stackItem.card
                    |> (\( id, content ) ->
                            { id = id
                            , content = content
                            , movement = stackItem.movement |> Tuple.mapBoth ((+) x) ((+) y)
                            , rotation = stackItem.rotation
                            , zIndex = stackItem.zIndex + i + 1
                            }
                       )
            )
        |> (::) (args.empty |> (\( id, content ) -> fromHtml ( x, y ) id content))


toHtml : List (Attribute msg) -> List (Entity msg) -> Html msg
toHtml attr list =
    list
        |> List.sortBy .id
        |> List.map
            (\entity ->
                ( entity.id
                , entity.content
                    [ Html.Attributes.style "position" "absolute"
                    , Html.Attributes.style "transition" "transform 0.5s"
                    , Game.Card.transform
                        [ Game.Card.move entity.movement
                        , Game.Card.rotate entity.rotation
                        ]
                    , Html.Attributes.style "z-index" (String.fromInt entity.zIndex)
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
