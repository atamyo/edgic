module Main exposing (..)

-- Keep track of edgic

import Browser
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Region as Region
import Http
import Json.Decode exposing (Decoder, decodeString, field, list, string)
import Keys exposing (key)
import RemoteData exposing (RemoteData(..), WebData)



-- MAIN


main : Program () Model Msg
main =
    Browser.document { init = init, update = update, subscriptions = subscriptions, view = view }



-- MODEL


type alias Model =
    { readSheetResponse : WebData String
    , contestants : List Contestant
    }


type alias Contestant =
    { name : Name
    , edgics : List Edgic
    }


type alias Name =
    String


type alias Edgic =
    String


init : () -> ( Model, Cmd Msg )
init _ =
    ( { readSheetResponse = Loading, contestants = [] }
    , Http.get
        { url = "https://sheets.googleapis.com/v4/spreadsheets/15ktMELDMRoU08C88m96h2aH-2Fka5hhYwHHl_4ws_-0/values/C2:I21?key=" ++ key
        , expect = Http.expectString (RemoteData.fromResult >> GotText)
        }
    )



-- UPDATE


type Msg
    = GotText (WebData String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotText response ->
            case response of
                Success data ->
                    ( { model | readSheetResponse = response, contestants = sheetResponseToContestants data }, Cmd.none )

                _ ->
                    ( { model | readSheetResponse = response, contestants = [] }, Cmd.none )


sheetResponseToContestants : String -> List Contestant
sheetResponseToContestants response =
    let
        rowToContestant row =
            case row of
                name :: edgics ->
                    Contestant name edgics

                [] ->
                    Contestant "Welp" [ "Welp" ]

        contestants rows =
            rows
                |> List.map rowToContestant
                |> List.filter (\contestant -> contestant /= Contestant "Welp" [ "Welp" ])
    in
    case decodeString sheetDecoder response of
        Ok rows ->
            contestants rows

        Err _ ->
            []


sheetDecoder : Decoder (List (List String))
sheetDecoder =
    field "values" (list (list string))



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> Browser.Document Msg
view model =
    let
        body =
            case model.readSheetResponse of
                Failure _ ->
                    text "Failed"

                Loading ->
                    text "Loading..."

                NotAsked ->
                    text "Not Asked"

                Success _ ->
                    column [ Font.color (rgb255 33 33 33), Font.size 18 ]
                        [ el [ Region.heading 1, Font.size 36, Font.semiBold, paddingXY 0 36 ] (text "✨🌈 Edgic Viewer")
                        , edgicsTable model.contestants
                        ]
    in
    { title = "Edgic Viewer", body = [ layout [] body ] }


edgicsTable : List Contestant -> Element Msg
edgicsTable contestants =
    table [ Region.mainContent, spacing 10 ]
        { data = contestants
        , columns =
            [ { header = none
              , width = fillPortion 1
              , view = \contestant -> el [ padding 7 ] (text contestant.name)
              }
            , { header = none
              , width = fillPortion 16
              , view = \contestant -> row [ spacing 10 ] (List.map edgicCell contestant.edgics)
              }
            ]
        }


edgicCell : String -> Element Msg
edgicCell edgic =
    el
        [ edgicBackgroundColor edgic
        , Font.center
        , Font.color (rgb255 255 255 255)
        , Border.rounded 3
        , padding 7
        , width (px 100)
        ]
        (text edgic)


edgicBackgroundColor : String -> Attribute Msg
edgicBackgroundColor edgic =
    case String.left 2 edgic of
        "IN" ->
            Background.color (rgb255 189 189 189)

        "UT" ->
            case String.slice 3 4 edgic of
                "P" ->
                    Background.color (rgb255 174 213 129)

                "M" ->
                    Background.color (rgb255 220 231 117)

                "N" ->
                    Background.color (rgb255 255 138 101)

                _ ->
                    Background.color (rgb255 255 218 190)

        "MO" ->
            case String.slice 3 4 edgic of
                "P" ->
                    Background.color (rgb255 142 36 170)

                "M" ->
                    Background.color (rgb255 251 140 0)

                "N" ->
                    Background.color (rgb255 94 53 177)

                _ ->
                    Background.color (rgb255 253 216 53)

        "CP" ->
            case String.slice 2 3 edgic of
                "P" ->
                    Background.color (rgb255 30 136 229)

                "M" ->
                    Background.color (rgb255 3 155 229)

                "N" ->
                    Background.color (rgb255 109 76 65)

                _ ->
                    Background.color (rgb255 0 172 193)

        "OT" ->
            case String.slice 3 4 edgic of
                "P" ->
                    Background.color (rgb255 192 202 51)

                "M" ->
                    Background.color (rgb255 240 98 146)

                "N" ->
                    Background.color (rgb255 229 57 53)

                _ ->
                    Background.color (rgb255 216 27 96)

        _ ->
            Background.color (rgb255 255 255 255)
