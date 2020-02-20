module View.WrappedColumn exposing (Model, Msg, init, jumpTo, subscriptions, syncPositions, update, view, viewButtonRow)

{-| Alternative Name: Scrolling Nav
-}

import Browser.Dom as Dom
import Element exposing (Element)
import Element.Font as Font
import Element.Input as Input
import Framework.Button as Button
import Framework.Card as Card
import Framework.Color as Color
import Framework.Grid as Grid
import Framework.Group as Group
import Framework.Heading as Heading
import Html.Attributes as Attributes
import IntDict exposing (IntDict)
import Task
import Time


type alias Model elem =
    { labels : elem -> String
    , positions : IntDict String
    , arrangement : List (List elem)
    , scrollPos : Int
    }


type Msg elm
    = GotHeaderPos elm (Result Dom.Error Int)
    | ChangedViewport (Result Dom.Error ())
    | JumpTo elm
    | SyncPosition Int
    | TimePassed


init :
    { labels : elem -> String
    , arrangement : List (List elem)
    }
    -> ( Model elem, Cmd (Msg elem) )
init { labels, arrangement } =
    { labels = labels
    , positions = IntDict.empty
    , arrangement = arrangement
    , scrollPos = 0
    }
        |> (\a ->
                ( a
                , syncPositions a
                )
           )


update : Msg elem -> Model elem -> ( Model elem, Cmd (Msg elem) )
update msg model =
    case msg of
        GotHeaderPos label result ->
            ( case result of
                Ok pos ->
                    { model
                        | positions =
                            model.positions
                                |> IntDict.insert pos
                                    (label |> model.labels)
                    }

                Err _ ->
                    model
            , Cmd.none
            )

        ChangedViewport _ ->
            ( model, Cmd.none )

        JumpTo elem ->
            ( model
            , model |> jumpTo elem
            )

        SyncPosition pos ->
            ( { model
                | scrollPos = pos
              }
            , Cmd.none
            )

        TimePassed ->
            ( model
            , Dom.getViewport
                |> Task.map (.viewport >> .y >> round)
                |> Task.perform SyncPosition
            )


subscriptions : Model elem -> Sub (Msg elem)
subscriptions _ =
    Time.every 100 (always TimePassed)


jumpTo : elem -> Model elem -> Cmd (Msg elem)
jumpTo elem { labels } =
    Dom.getElement (elem |> labels)
        |> Task.andThen
            (\{ element } ->
                Dom.setViewport 0 element.y
            )
        |> Task.attempt ChangedViewport


syncPositions : Model elem -> Cmd (Msg elem)
syncPositions { labels, arrangement } =
    arrangement
        |> List.concat
        |> List.map
            (\label ->
                Dom.getElement (labels label)
                    |> Task.map
                        (.element
                            >> .y
                            >> round
                        )
                    |> Task.attempt
                        (GotHeaderPos label)
            )
        |> Cmd.batch


viewButtonRow : Model elm -> Element (Msg elm)
viewButtonRow { arrangement, scrollPos, labels, positions } =
    let
        current =
            positions
                |> IntDict.before (scrollPos + 1)
                |> Maybe.map Tuple.second
    in
    arrangement
        |> List.concat
        |> List.map
            (\name ->
                Input.button
                    (Button.fill
                        ++ Group.center
                        ++ (if Just (labels name) == current then
                                Color.primary

                            else
                                []
                           )
                        ++ [ Element.height <| Element.fill
                           , Font.size <| 10
                           ]
                    )
                    { onPress = Just <| JumpTo <| name
                    , label = Element.text <| labels <| name
                    }
            )
        |> Element.row
            (Card.large
                ++ Group.top
                ++ [ Element.alignBottom
                   , Element.padding 0
                   , Element.centerX
                   ]
            )


view :
    (elem -> Element msg)
    -> Model elem
    -> Element msg
view asElement { labels, arrangement } =
    arrangement
        |> List.map
            (List.map
                (\header ->
                    Element.column Grid.simple <|
                        [ Element.el
                            (Heading.h2
                                ++ [ header
                                        |> labels
                                        |> Attributes.id
                                        |> Element.htmlAttribute
                                   ]
                            )
                          <|
                            Element.text <|
                                (header |> labels)
                        , header |> asElement
                        ]
                )
                >> Element.column Grid.simple
            )
        |> Element.wrappedRow Grid.simple
