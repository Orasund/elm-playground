module GJumper.Core exposing (Footer(..), Gui(..), Header(..), View(..), create, toAreas, withFooter, withHeader)

import PixelEngine exposing (Area, Background)
import PixelEngine.Image exposing (Image)
import PixelEngine.Tile exposing (Tile, Tileset)


type View square
    = View
        { gui : ( Header, Footer )
        , player : Tile Never
        , square : square -> Tile Never
        , tileset : Tileset
        , background :
            { grid : Background
            , gui : Background
            }
        }


type Gui
    = Gui
        { background : Background
        , tileset : Tileset
        , header : Header
        , body : ( List ( ( Int, Int ), Tile Never ), Background )
        , footer : Footer
        }


type Header
    = Header (List ( Float, Image Never ))


type Footer
    = Footer (List ( ( Float, Float ), Image Never ))


create : { gui : Background, grid : Background } -> Tileset -> List ( ( Int, Int ), Tile Never ) -> Gui
create background tileset list =
    Gui
        { tileset = tileset
        , background = background.gui
        , header = Header []
        , body = ( list, background.grid )
        , footer = Footer []
        }


withHeader : Header -> Gui -> Gui
withHeader header (Gui gui) =
    Gui { gui | header = header }


withFooter : Footer -> Gui -> Gui
withFooter footer (Gui gui) =
    Gui { gui | footer = footer }


toAreas : Int -> Gui -> List (Area Never)
toAreas imgSize (Gui ({ background, tileset, body } as gui)) =
    let
        ( Footer footer, Header header ) =
            ( gui.footer, gui.header )

        ( bodyList, bodyBackground ) =
            body
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
        |> List.map (\( ( x, y ), img ) -> ( ( x, y * toFloat imgSize ), img ))
        |> PixelEngine.imageArea
            { height = toFloat imgSize * 3
            , background = background
            }
    ]
