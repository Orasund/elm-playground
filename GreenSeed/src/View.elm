module View exposing (..)

import Html exposing (Html)


stylesheet : Html msg
stylesheet =
    """
.button,button {
    border : 0;
    background: color-mix(in lab,white,black 20%);
}

.button:hover,button:hover {
    filter: brightness(0.8);
}

.button:active,button:active {
    filter: brightness(0.6);
}

body {
    height:100%;
    width:100%;
    --color-green:green;
}

.icon-button {
    border-radius: 100%;
    aspect-ratio:1;
    padding: 8px;
}
    """
        |> Html.text
        |> List.singleton
        |> Html.node "style" []
