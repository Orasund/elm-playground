module PixelEngine exposing (Background(..), Tile, Tileset, animatedTile, render, tile, tiledArea)

import Css exposing (px)
import Css.Foreign as Foreign
import Html.Styled exposing (Html, div, img)
import Html.Styled.Attributes exposing (css, src)


type alias Location =
    ( Int, Int )


type alias Tile =
    { top : Int
    , left : Int
    , steps : Int
    }


type alias Tileset =
    { source : String
    , width : Int
    , height : Int
    }


type Background
    = Color Css.Color
    | Image String


type alias TiledArea =
    { height : Int
    , tileset : Tileset
    , background : Background
    , content : List ( Location, Tile )
    }


type Area
    = Tiled TiledArea


type alias Config =
    { scale : Int
    , width : Int
    }


tiledArea : { height : Int, tileset : Tileset, background : Background } -> List ( Location, Tile ) -> Area
tiledArea { height, tileset, background } content =
    Tiled
        { height = height
        , tileset = tileset
        , background = background
        , content = content
        }


tile : Location -> Tile
tile ( left, top ) =
    { top = top, left = left, steps = 0 }


animatedTile : Location -> Int -> Tile
animatedTile ( left, top ) steps =
    { top = top
    , left = left
    , steps =
        if steps > 0 then
            steps
        else
            0
    }


render : Config -> List Area -> Html msg
render config listOfArea =
    div [ css [ Css.backgroundColor (Css.rgb 0 0 0) ] ]
        (listOfArea
            |> List.foldl
                (\area list ->
                    case area of
                        Tiled tiledArea ->
                            [ renderTiledArea config tiledArea ]
                                |> List.append list
                )
                [ Foreign.global
                    [ Foreign.selector
                        "@keyframes pixelengine_graphics_basic"
                        [ Css.property "100% { right:0px };" "" ]
                    ]
                ]
        )


renderTiledArea : Config -> TiledArea -> Html msg
renderTiledArea { scale, width } { height, background, content, tileset } =
    div
        [ css
            (let
                style =
                    [ Css.width <| px <| toFloat <| scale * tileset.width * width
                    , Css.height <| px <| toFloat <| scale * tileset.height * height
                    , Css.margin Css.auto
                    , Css.position Css.relative
                    ]
             in
             case background of
                Color color ->
                    List.append style
                        [ Css.backgroundColor color ]

                Image image ->
                    List.append style
                        [ Css.backgroundImage (Css.url image)
                        , Css.backgroundRepeat Css.repeat
                        , Css.backgroundSize2 (px <| toFloat <| scale * tileset.width) (px <| toFloat <| scale * tileset.height)
                        , Css.property "image-rendering" "pixelated"
                        ]
            )
        ]
        (content
            |> List.map (displayTile tileset scale)
        )


displayTile : Tileset -> Int -> ( Location, Tile ) -> Html msg
displayTile { width, height, source } scale ( pos, tile ) =
    let
        ( x, y ) =
            pos

        ( i, j ) =
            ( tile.left, tile.top )
    in
    if tile.steps == 0 then
        img
            [ src source
            , css
                [ Css.property "object-fit" "none"
                , Css.property
                    "object-position"
                    (toString (-1 * width * i) ++ "px " ++ toString (-1 * height * j) ++ "px")
                , Css.width <| px <| toFloat <| width
                , Css.height <| px <| toFloat <| height
                , Css.position Css.absolute
                , Css.top <| px <| toFloat <| scale * height * y + height // 2
                , Css.left <| px <| toFloat <| scale * width * x + width // 2
                , Css.property "image-rendering" "pixelated"
                , Css.transform <| Css.scale2 scale scale
                ]
            ]
            []
    else
        div
            [ css
                [ Css.position Css.absolute
                , Css.top <| px <| toFloat <| scale * height * y
                , Css.left <| px <| toFloat <| scale * width * x
                , Css.width <| px <| toFloat <| scale * width
                , Css.height <| px <| toFloat <| scale * height
                , Css.overflow Css.hidden
                ]
            ]
            [ img
                [ src source
                , css
                    [ Css.property "object-fit" "none"
                    , Css.property
                        "object-position"
                        (toString (-1 * width * (i - 1)) ++ "px " ++ toString (-1 * height * j) ++ "px")
                    , Css.width <| px <| toFloat <| width * (tile.steps + 2)
                    , Css.height <| px <| toFloat <| height
                    , Css.position Css.relative
                    , Css.right <| px <| toFloat <| width * 2 * (tile.steps + 1)
                    , Css.marginLeft <| px <| toFloat <| width * (tile.steps + 2) // 2
                    , Css.top <| px <| toFloat <| height // 2
                    , Css.property "image-rendering" "pixelated"
                    , Css.property "animation" ("pixelengine_graphics_basic " ++ toString (tile.steps + 1) ++ ".0s steps(" ++ toString (tile.steps + 1) ++ ") infinite")
                    , Css.transform <| Css.scale2 scale scale
                    ]
                ]
                []
            ]
