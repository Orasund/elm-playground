module HighschoolDrama.Page.Title exposing (Model, Msg, init, update, view)

import Action
import Element
import Element.Input as Input
import HighschoolDrama.Data as Data exposing (Options, Sex(..))
import Html exposing (Html)
import Random exposing (Seed)


type GroupsForm
    = Choosing
    | BoysAndGirls ( Int, Int ) Sex
    | AllEqual Int


type alias Model =
    { groupsForm : GroupsForm
    , seed : Seed
    }


type Msg
    = SelectSex (Maybe Sex)
    | StartGame


type alias Action =
    Action.Action Model Never Options (Maybe Sex)


init : Seed -> ( Model, Cmd Msg )
init seed =
    ( { seed = seed
      , groupsForm = Choosing
      }
    , Cmd.none
    )


submitForm : Model -> Maybe Options
submitForm ({ seed, groupsForm } ) =
    case groupsForm of
        BoysAndGirls ( boys, girls ) sex ->
            Just
                { seed = seed
                , groups =
                    Data.BoysAndGirls
                        { boys = boys
                        , girls = girls
                        , you = sex
                        }
                }

        AllEqual num ->
            Just
                { seed = seed
                , groups =
                    Data.AllEqual
                        num
                }

        _ ->
            Nothing


update : Msg -> Model -> Action
update msg model =
    case msg of
        SelectSex maybeSex ->
            Action.transitioning maybeSex
                |> Debug.todo "translate to Options"

        StartGame ->
            model
                |> submitForm
                |> Maybe.map Action.transitioning
                |> Maybe.withDefault (Action.updating ( model, Cmd.none ))


view : Model -> List (Html Msg)
view { groupsForm } =
    List.singleton <|
        Element.layout [] <|
            Element.column [ Element.centerX ] <|
                [ Input.radio []
                    { onChange = SelectSex
                    , selected = Nothing
                    , label = Input.labelAbove [] <| Element.text "Choose a Side:"
                    , options =
                        [ Input.option (Just Male) <| Element.text "Boys"
                        , Input.option (Just Male) <| Element.text "Girls"
                        , Input.option Nothing <| Element.text "Switch off mechanics based on sex"
                        ]
                    }
                , case groupsForm of
                    Choosing ->
                        Element.text "Choose one of the above to see advanced options"

                    _ ->
                        Element.text "TODO"
                ]
