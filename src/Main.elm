module Main exposing (..)

-- Keep track of edgic

import Browser
import Html exposing (Html, li, text, ul)
import Http
import Json.Decode exposing (Decoder, decodeString, field, list, string)
import Keys exposing (key)



-- MAIN


main : Program () Model Msg
main =
    Browser.element { init = init, update = update, subscriptions = subscriptions, view = view }



-- MODEL


type Model
    = Failure
    | Loading
    | Success String


init : () -> ( Model, Cmd Msg )
init _ =
    ( Loading
    , Http.get
        { url = "https://sheets.googleapis.com/v4/spreadsheets/15ktMELDMRoU08C88m96h2aH-2Fka5hhYwHHl_4ws_-0/values/C2:I21?key=" ++ key
        , expect = Http.expectString GotText
        }
    )



-- UPDATE


type Msg
    = GotText (Result Http.Error String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg _ =
    case msg of
        GotText result ->
            case result of
                Ok text ->
                    ( Success text, Cmd.none )

                _ ->
                    ( Failure, Cmd.none )


sheetDecoder : Decoder (List (List String))
sheetDecoder =
    field "values" (list (list string))



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    case model of
        Failure ->
            text "Failed"

        Loading ->
            text "Loading..."

        Success response ->
            edgicView response


edgicView : String -> Html Msg
edgicView response =
    let
        decoded =
            decodeString sheetDecoder response

        result =
            case decoded of
                Ok rows ->
                    rows

                Err _ ->
                    [ [ "Decode error" ] ]

        displayedRows =
            List.map (\row -> li [] [ text <| String.join " " row ]) result
    in
    ul [] displayedRows
