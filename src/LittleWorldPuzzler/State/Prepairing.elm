module LittleWorldPuzzler.State.Prepairing exposing (Model, Msg(..), update)

import Action
import LittleWorldPuzzler.Data.CellType exposing (CellType(..))
import LittleWorldPuzzler.Data.Deck exposing (Selected(..))
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


type alias Action =
    Action.Action Model
        Never
        { scale : Float
        , seed : Seed
        , portraitMode : Bool
        }
        Never



--()
----------------------
-- Update
----------------------


update : Msg -> Model -> Action
update msg model =
    case msg of
        GotSeed seed ->
            case model.scale of
                Just scale ->
                    Action.transitioning
                        { scale = scale
                        , portraitMode = model.portraitMode
                        , seed = seed
                        }

                Nothing ->
                    Action.updating
                        ( { model | seed = Just seed }
                        , Cmd.none
                        )
