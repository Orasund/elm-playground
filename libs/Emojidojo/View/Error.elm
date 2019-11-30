module Emojidojo.View.Error exposing (view)

import Element exposing (Element)
import Emojidojo.Error as Error
import Framework.Card as Card
import Framework.Color as Color
import Http exposing (Error)


view : Error -> Element msg
view error =
    Element.el (Card.fill ++ Color.danger) <|
        Element.text (error |> Error.toString)
