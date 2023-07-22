module Dir exposing (..)


type alias Dir =
    ( Int, Int, String )


new : ( Int, Int ) -> Dir
new ( x, y ) =
    ( x, y, "Dir" )


add : Dir -> ( Int, Int ) -> ( Int, Int )
add dir ( x2, y2 ) =
    let
        ( x1, y1, _ ) =
            dir
    in
    ( x1 + x2, y1 + y2 )


reverse : Dir -> Dir
reverse ( x, y, _ ) =
    new ( -x, -y )
