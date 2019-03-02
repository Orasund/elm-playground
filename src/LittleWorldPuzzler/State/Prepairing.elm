module LittleWorldPuzzler.State.Prepairing exposing (Model, Msg(..), update)

import Framework.Modifier exposing (Modifier(..))
import LittleWorldPuzzler.Data.CellType exposing (CellType(..))
import LittleWorldPuzzler.Data.Deck exposing (Selected(..))
import LittleWorldPuzzler.State as State exposing (Action(..))
import Random exposing (Seed)



----------------------
-- Model
----------------------


type alias Model =
    { scale : Maybe Float
    , portraitMode : Bool
    , seed : Maybe Seed
    }


type Msg
    = GotSeed Seed



----------------------
-- Update
----------------------


update : Msg -> Model -> Action Model Msg { scale : Float, seed : Seed, portraitMode : Bool }
update msg model =
    case msg of
        GotSeed seed ->
            case model.scale of
                Just scale ->
                    Transition
                        { scale = scale
                        , portraitMode = model.portraitMode
                        , seed = seed
                        }

                Nothing ->
                    Update
                        ( { model | seed = Just seed }
                        , Cmd.none
                        )
