module AsteroidMiner.View.GUI exposing (Blueprint(..), Model, Msg, init, update, view)

import AsteroidMiner.Data exposing (size, spriteSize)
import AsteroidMiner.View.Tileset.Big as Tileset
import Location exposing (Location)
import PixelEngine.Image as Image exposing (Image)


type Blueprint
    = ConveyorBelt
    | Container


type alias Model =
    { selected : Blueprint }


type Msg
    = ItemSelected Blueprint


init : Model
init =
    { selected = ConveyorBelt
    }


update : Msg -> Model -> Model
update msg model =
    case msg of
        ItemSelected blueprint ->
            { model | selected = blueprint }


viewList : List (Image msg) -> List ( Location, Image msg )
viewList list =
    let
        length : Float
        length =
            list
                |> List.length
                |> toFloat

        center : Float
        center =
            toFloat size * spriteSize / 2
    in
    list
        |> List.indexedMap
            (\i image ->
                ( ( center - spriteSize - (length * spriteSize / 2) + toFloat i * spriteSize * 2
                  , spriteSize / 2
                  )
                , image
                )
            )


viewBlueprint : Model -> Blueprint -> Image Msg
viewBlueprint { selected } blueprint =
    let
        { image, symobl } =
            case blueprint of
                ConveyorBelt ->
                    Tileset.conveyorBelt

                Container ->
                    Tileset.container
    in
    if blueprint == selected then
        image

    else
        symobl
            |> Image.clickable (ItemSelected blueprint)


view : Model -> List ( Location, Image Msg )
view model =
    [ ConveyorBelt, Container ]
        |> List.map (viewBlueprint model)
        |> viewList
