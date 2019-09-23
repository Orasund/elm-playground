module GJumper.Core exposing (Footer(..), Gui(..), Header(..), View(..), create, gridGenerator, toAreas, withFooter, withHeader)

import Grid exposing (Grid)
import PixelEngine exposing (Area, Background)
import PixelEngine.Image exposing (Image)
import PixelEngine.Tile exposing (Tile, Tileset)
import Random exposing (Generator)
import Random.List as Random


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



-----------------------------------------------------------------------
-- Grid
-----------------------------------------------------------------------


gridGenerator :
    a
    ->
        { distribution : a -> ( ( Float, Maybe square ), List ( Float, Maybe square ) )
        , fixed : a -> List ( Int, square )
        , level : a -> List (List (Maybe square))
        , rows : Int
        , columns : Int
        }
    -> Generator (Grid square)
gridGenerator l { distribution, fixed, level, rows, columns } =
    let
        distributedBoard : Generator (Grid square)
        distributedBoard =
            level l
                |> List.indexedMap (\y -> List.indexedMap (\x square -> ( ( x, y ), square )))
                |> List.concat
                |> List.filterMap
                    (\( loc, maybeSquare ) -> maybeSquare |> Maybe.map (\square -> ( loc, square )))
                |> Grid.fromList
                    { rows = rows
                    , columns = columns
                    }
                |> (\g ->
                        Random.list (rows * columns)
                            (distribution l |> (\( a, b ) -> Random.weighted a b))
                            |> Random.map
                                (List.indexedMap (\i s -> ( ( i |> modBy columns, i // columns ), s ))
                                    >> List.filterMap (\( pos, maybeS ) -> maybeS |> Maybe.map (\s -> ( pos, s )))
                                    >> List.foldl
                                        (\( pos, square ) ->
                                            Grid.update pos
                                                (Maybe.map Just
                                                    >> Maybe.withDefault (Just <| square)
                                                )
                                        )
                                        g
                                )
                   )
    in
    fixed l
        |> List.map (\( n, s ) -> List.repeat n s)
        |> List.concat
        |> List.foldl
            (\square ->
                Random.map
                    (\( b, positions ) ->
                        case positions of
                            [] ->
                                ( b, positions )

                            pos :: list ->
                                ( b |> Grid.insert pos square, list )
                    )
            )
            (distributedBoard
                |> Random.andThen
                    (\b ->
                        Random.pair distributedBoard
                            (b
                                |> Grid.emptyPositions
                                |> Random.shuffle
                                |> Random.andThen Random.shuffle
                            )
                    )
            )
        |> Random.map Tuple.first
