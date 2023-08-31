module Game exposing (..)

import Cell exposing (Cell)
import Config
import Crop exposing (Crop(..))
import Dict exposing (Dict)


type alias Game =
    { field : Dict ( Int, Int ) Cell
    , forSale : List Crop
    , money : Int
    }


setFieldOf : Game -> Dict ( Int, Int ) Cell -> Game
setFieldOf game field =
    { game | field = field }


new =
    { field = Dict.empty
    , forSale = [ Tomato, Carrot, Beans, Clover ]
    , money = 10
    }


buyAndPlace : ( Int, Int ) -> Crop -> Game -> Maybe Game
buyAndPlace pos crop game =
    if Crop.price crop <= game.money then
        let
            cell =
                Dict.get pos game.field |> Maybe.withDefault Cell.soil
        in
        if cell.soilHealth >= Crop.soilHealthNeeded crop then
            game.field
                |> Dict.insert pos (cell |> Cell.addCrop crop)
                |> setFieldOf { game | money = game.money - Crop.price crop }
                |> Just

        else
            Nothing

    else
        Nothing


cropEffect : ( Int, Int ) -> Crop -> Game -> Game
cropEffect pos crop game =
    case crop of
        Clover ->
            game.field
                |> Dict.update pos
                    (Maybe.map
                        (\cell ->
                            { cell | soilHealth = Config.maxSoilHealth }
                        )
                    )
                |> setFieldOf game

        _ ->
            game


dayPassed : Game -> Game
dayPassed g =
    Dict.foldl
        (\pos cell game ->
            case cell.crop of
                Just crop ->
                    if cell.age + 1 == Crop.maxAge crop then
                        game.field
                            |> Dict.insert pos
                                (Cell.soilWithHealth (cell.soilHealth - Crop.soilHealthNeeded crop))
                            |> setFieldOf { game | money = game.money + Crop.price crop * 2 }
                            |> cropEffect pos crop

                    else
                        game.field
                            |> Dict.insert pos { cell | age = cell.age + 1 }
                            |> setFieldOf game

                Nothing ->
                    game
        )
        g
        g.field
