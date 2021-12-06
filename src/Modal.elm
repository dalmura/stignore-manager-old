module Modal exposing (new)

import Html exposing (Html, Attribute, div, h2, text)
import Html.Attributes exposing (style, id)
import Html.Events exposing (onClick)


maskStyle : List (Attribute msg)
maskStyle =
    [ style "background-color" "rgba(0,0,0,0.3)"
    , style "position" "fixed"
    , style "top" "0"
    , style "left" "0"
    , style "width" "100%"
    , style "height" "100%"
    , style "z-index" "1"
    ]


modalStyle : List (Attribute msg)
modalStyle =
    [ style "background-color" "rgba(255,255,255,1.0)"
    , style "position" "absolute"
    , style "top" "50%"
    , style "left" "50%"
    , style "height" "auto"
    , style "max-height" "80%"
    , style "width" "700px"
    , style "max-width" "95%"
    , style "padding" "10px"
    , style "border-radius" "3px"
    , style "box-shadow" "1px 1px 5px rgba(0,0,0,0.5)"
    , style "transform" "translate(-50%, -50%)"
    ]


new : String -> String -> List (Html msg) -> msg -> Html msg
new divId modalTitle modalBody closeMsg =
  div
    (maskStyle ++ [ id divId, onClick closeMsg ])
    [ div
        modalStyle
        [ h2 [] [ text modalTitle ]
        , div [] modalBody
        ]
    ]
