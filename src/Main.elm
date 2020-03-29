module Main exposing (..)

-- Keep track of edgic

import Browser
import Element exposing (..)
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
    , edgic : Edgic
    }


type alias Name =
    String


type alias Edgic =
    List String


type Rating
    = INV
    | UTR
    | MOR
    | CP
    | OTT
    | NoRating


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
                    ( { model | readSheetResponse = response, contestants = parseResponse data }, Cmd.none )

                _ ->
                    ( { model | readSheetResponse = response, contestants = [] }, Cmd.none )


parseResponse : String -> List Contestant
parseResponse response =
    let
        rowToContestant row =
            case row of
                name :: edgic ->
                    Contestant name edgic

                [] ->
                    Contestant "Welp" [ "Welp" ]

        contestants rows =
            rows
                |> List.map (\row -> rowToContestant row)
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
                    edgicView model.contestants
    in
    { title = "Edgic Viewer", body = [ layout [] body ] }


edgicView : List Contestant -> Element Msg
edgicView contestants =
    let
        nameToElement name =
            el [ width (px 100) ] (text name)

        edgicToElements : List String -> List (Element Msg)
        edgicToElements edgic =
            List.map edgicCell edgic

        contestantToRow contestant =
            row [ padding 10, spacing 7 ] (nameToElement contestant.name :: edgicToElements contestant.edgic)

        displayedRows =
            List.map (\contestant -> contestantToRow contestant) contestants
    in
    textColumn [] displayedRows


edgicCell : String -> Element Msg
edgicCell edgic =
    el [ width (px 100) ] (text edgic)
