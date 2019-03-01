module LittleWorldPuzzler.State.Prepairing exposing (Model, Msg(..), update)

import Framework.Modifier exposing (Modifier(..))
import LittleWorldPuzzler.Data.CellType exposing (CellType(..))
import LittleWorldPuzzler.Data.Deck exposing (Selected(..))
import Random exposing (Seed)



----------------------
-- Model
----------------------


type alias Model =
    { scale : Maybe Float
    , seed : Maybe Seed
    }


type Msg
    = GotSeed Seed



----------------------
-- Update
----------------------


update :
    (Float -> Seed -> model)
    -> (Model -> model)
    -> Msg
    -> Model
    -> ( model, Cmd Msg )
update startPlaying modelMapper msg model =
    case msg of
        GotSeed seed ->
            case model.scale of
                Just scale ->
                    ( startPlaying scale seed
                    , Cmd.none
                    )

                Nothing ->
                    ( modelMapper { model | seed = Just seed }
                    , Cmd.none
                    )
