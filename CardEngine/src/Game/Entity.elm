module Game.Entity exposing (..)

import Game.Card exposing (Transformation)
import Html exposing (Attribute, Html, a)
import Html.Attributes


type alias Entity a =
    { position : ( Float, Float )
    , rotation : Float
    , customTransformations : List Transformation
    , zIndex : Int
    , content : a
    }


new : a -> Entity a
new a =
    { position = ( 0, 0 )
    , rotation = 0
    , customTransformations = []
    , zIndex = 1
    , content = a
    }


map : (a -> b) -> Entity a -> Entity b
map fun i =
    { position = i.position
    , rotation = i.rotation
    , content = fun i.content
    , customTransformations = i.customTransformations
    , zIndex = i.zIndex
    }


attributes : Entity a -> List (Attribute msg)
attributes entity =
    [ [ Game.Card.move entity.position
      , Game.Card.rotate entity.rotation
      ]
        ++ entity.customTransformations
        |> Game.Card.transform
    , Html.Attributes.style "z-index" (String.fromInt entity.zIndex)
    ]


toHtml : (a -> List (Attribute msg) -> Html msg) -> Entity a -> Html msg
toHtml fun entity =
    fun entity.content (attributes entity)


withRotation : Float -> Entity a -> Entity a
withRotation float entity =
    { entity | rotation = float }


withPosition : ( Float, Float ) -> Entity a -> Entity a
withPosition pos entity =
    { entity | position = pos }


withZIndex : Int -> Entity a -> Entity a
withZIndex int entity =
    { entity | zIndex = int }


withCustomTransformations : List Transformation -> Entity a -> Entity a
withCustomTransformations list entity =
    { entity | customTransformations = list }


mapRotation : (Float -> Float) -> Entity a -> Entity a
mapRotation fun entity =
    withRotation (fun entity.rotation) entity


mapPosition : (( Float, Float ) -> ( Float, Float )) -> Entity a -> Entity a
mapPosition fun entity =
    withPosition (fun entity.position) entity


mapZIndex : (Int -> Int) -> Entity a -> Entity a
mapZIndex fun entity =
    withZIndex (fun entity.zIndex) entity


mapCustomTransformations : (List Transformation -> List Transformation) -> Entity a -> Entity a
mapCustomTransformations fun entity =
    withCustomTransformations (fun entity.customTransformations) entity
