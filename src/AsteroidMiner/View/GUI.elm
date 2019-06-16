module AsteroidMiner.View.GUI exposing (Model, Msg, Tool(..), init, toDefault, update, view)

import AsteroidMiner.Data exposing (size, spriteSize)
import AsteroidMiner.View.Tileset.Big as Tileset
import Location exposing (Location)
import PixelEngine.Image as Image exposing (Image)


type Tool
    = Mine
    | ConveyorBelt
    | Container
    | Delete


type alias Model =
    { selected : Tool }


type Msg
    = ItemSelected Tool


init : Model
init =
    { selected = ConveyorBelt
    }


toDefault : Model -> Model
toDefault =
    always init


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
                ( ( center
                        - spriteSize
                        - (length * spriteSize / 2)
                        + (toFloat i * spriteSize * 2)
                  , spriteSize / 2
                  )
                , image
                )
            )


viewBlueprint : Model -> Tool -> Image Msg
viewBlueprint { selected } blueprint =
    let
        { image, symobl } =
            case blueprint of
                Mine ->
                    Tileset.mine

                ConveyorBelt ->
                    Tileset.conveyorBelt

                Container ->
                    Tileset.container

                Delete ->
                    Tileset.delete
    in
    if blueprint == selected then
        image

    else
        symobl
            |> Image.clickable (ItemSelected blueprint)


view : Model -> List ( Location, Image Msg )
view model =
    [ Mine, ConveyorBelt, Delete, Container ]
        |> List.map (viewBlueprint model)
        |> viewList
