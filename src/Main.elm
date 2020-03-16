module Main exposing (..)

-- Keep track of edgic

import Browser
import Html exposing (Html, li, text, ul)
import Html.Events exposing (onClick)



-- MAIN


main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
    List String


init : Model
init =
    []



-- UPDATE


type Msg
    = NoOp


update : Msg -> Model -> Model
update msg model =
    case msg of
        NoOp ->
            model



-- VIEW


view : Model -> Html Msg
view model =
    ul []
        [ li [] <| List.intersperse (text " ") [ text "Parvati", text "CPP4", text "MOR3" ]
        , li [] <| List.intersperse (text " ") [ text "Michele", text "UTR2", text "UTR2" ]
        ]
