module Game.Gui exposing (Gui,create,withHeader,withFooter,toAreas)

import PixelEngine exposing (Background,Area)
import PixelEngine.Image exposing (Image)
import PixelEngine.Tile exposing (Tile,Tileset)

type alias Gui =
    { background : Background
    , tileset : Tileset
    , header : List ( Float, Image Never )
    , body : (List ( ( Int, Int ), Tile Never ),Background)
    , footer : List ( ( Float, Float ), Image Never )
    }

create : {gui:Background,grid:Background} -> Tileset -> List ( ( Int, Int ), Tile Never ) -> Gui
create background tileset list=
    { tileset = tileset
    , background = background.gui
    , header = []
    , body = (list,background.grid)
    , footer = []
    }

withHeader : List ( Float, Image Never ) -> Gui -> Gui
withHeader header gui =
    {gui | header = header}

withFooter : List ( Float, Image Never ) -> List ( Float, Image Never ) -> List ( Float, Image Never )  -> Gui -> Gui
withFooter l1 l2 l3 gui =
    let
        mapList : Float -> List ( Float, Image Never ) -> List ( (Float,Float), Image Never )
        mapList y =
            List.map (\(x,img) -> ((x,y),img))
    in
    {gui
    | footer =[ l1 |> mapList 0
    , l2 |> mapList 1
    , l3 |> mapList 2
    ]
    |> List.concat
    }
    
toAreas : Int -> Gui -> List (Area Never)
toAreas imgSize {background,tileset,header,body,footer} =
    let
        (bodyList,bodyBackground) = body
    in
    [ header
        |> List.map (\( y, img ) -> ( ( 0, y ), img ))
        |> PixelEngine.imageArea
            { height = toFloat imgSize
            , background = background
            }
    , bodyList
        |> PixelEngine.tiledArea
            { rows = 16
            , tileset = tileset
            , background = bodyBackground
            }
    , footer
        |> List.map (\((x,y),img) -> ((x,y* toFloat imgSize),img))
        |> PixelEngine.imageArea
            { height = toFloat imgSize*3
            , background = background
            }
    ]