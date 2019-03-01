module LittleWorldPuzzler.State.Replaying exposing (Model, Msg, update, view)

import Element exposing (Element)
import Element.Border as Border
import Element.Font as Font
import Framework.Modifier exposing (Modifier(..))
import LittleWorldPuzzler.Data.CellType exposing (CellType(..))
import LittleWorldPuzzler.Data.Deck exposing (Selected(..))
import LittleWorldPuzzler.Data.Game exposing (Game)
import LittleWorldPuzzler.View.Button as Button
import LittleWorldPuzzler.View.Game as GameView
import UndoList exposing (UndoList)



----------------------
-- Model
----------------------


type alias Model =
    UndoList Game


type Msg
    = Next
    | Previous



----------------------
-- Update
----------------------


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Next ->
            ( model |> UndoList.redo, Cmd.none )

        Previous ->
            ( model |> UndoList.undo, Cmd.none )



----------------------
-- View
----------------------


view : Float -> msg -> (Msg -> msg) -> Model -> Element msg
view scale restartMsg msgMapper model =
    let
        ({ score } as game) =
            model.present
    in
    Element.column
        [ Element.centerY
        , Element.centerX
        , Element.spacing 5
        ]
        [ Element.row
            [ Element.spaceEvenly
            , Element.centerX
            , Element.width <| Element.px <| floor <| 608 * scale
            ]
          <|
            [ Element.row
                [ Element.width <| Element.px <| floor <| 150 * scale
                , Element.spacing 5
                ]
              <|
                [ Button.view
                    [ Element.padding <| floor <| 7 * scale
                    , Border.rounded (floor <| 10 * scale)
                    , Font.size <| floor <| 36 * scale
                    , Element.width <| Element.px <| floor <| 36 * scale
                    , Font.family
                        [ Font.sansSerif ]
                    ]
                    { onPress = Just (msgMapper Previous)
                    , label = Element.text "<"
                    }
                , Button.view
                    [ Element.padding <| floor <| 7 * scale
                    , Border.rounded (floor <| 10 * scale)
                    , Element.width <| Element.px <| floor <| 36 * scale
                    , Font.size <| floor <| 36 * scale
                    , Font.family
                        [ Font.sansSerif ]
                    ]
                    { onPress = Just (msgMapper Next)
                    , label = Element.text ">"
                    }
                ]
            , Element.el [ Font.size <| floor <| 50 * scale ] <|
                Element.text <|
                    String.fromInt score
            , Button.view
                [ Element.width <| Element.px <| floor <| 150 * scale
                , Element.padding <| floor <| 7 * scale
                , Border.rounded (floor <| 10 * scale)
                , Font.size <| floor <| 36 * scale
                , Font.family
                    [ Font.sansSerif ]
                ]
                { onPress = Just restartMsg
                , label = Element.text "Restart"
                }
            ]
        , GameView.view
            { scale = scale
            , selected = Nothing
            , restartMsg = restartMsg
            , status = Nothing
            , highscore = Nothing
            }
            Nothing
            game
        ]
