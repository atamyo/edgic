module Main exposing (..)

-- Keep track of edgic

import Browser
import Html exposing (Attribute, Html, button, div, li, text, ul)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)



-- MAIN


main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
    { contestants : List Contestant
    , selectedRating : Rating
    , selectedTone : Tone
    , selectedVisibility : Visibility
    }


type alias Contestant =
    { name : String
    , edgics : List String
    }


type Rating
    = INV
    | UTR
    | MOR
    | CP
    | OTT
    | RatingUnselected


type Tone
    = NN
    | N
    | M
    | P
    | PP
    | ToneUnselected


type Visibility
    = One
    | Two
    | Three
    | Four
    | Five
    | VisibilityUnselected


init : Model
init =
    { contestants = initialContestants
    , selectedRating = RatingUnselected
    , selectedTone = ToneUnselected
    , selectedVisibility = VisibilityUnselected
    }


initialContestants : List Contestant
initialContestants =
    [ Contestant "Parvati" [], Contestant "Michele" [] ]



-- UPDATE


type Msg
    = AddEdgic Contestant
    | SelectRating Rating
    | SelectTone Tone
    | SelectVisibility Visibility


update : Msg -> Model -> Model
update msg model =
    case msg of
        AddEdgic contestant ->
            let
                newEdgic =
                    ratingToString model.selectedRating ++ toneToString model.selectedTone ++ visibilityToString model.selectedVisibility

                contestantUpdatedEdgics =
                    List.append contestant.edgics <| List.singleton newEdgic

                updatedContestant =
                    { contestant | edgics = contestantUpdatedEdgics }

                contestants =
                    model.contestants
                        |> List.filter (\c -> c.name /= updatedContestant.name)
            in
            { model | contestants = updatedContestant :: contestants, selectedRating = RatingUnselected, selectedTone = ToneUnselected, selectedVisibility = VisibilityUnselected }

        SelectRating rating ->
            { model | selectedRating = rating }

        SelectTone tone ->
            { model | selectedTone = tone }

        SelectVisibility visibility ->
            { model | selectedVisibility = visibility }



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ ul [] <| List.map edgicRow model.contestants
        , addView model
        ]


edgicRow : Contestant -> Html Msg
edgicRow contestant =
    contestant.edgics
        |> List.map text
        |> List.append [ addButton contestant, text contestant.name ]
        |> List.intersperse (text " ")
        |> li []


addButton : Contestant -> Html Msg
addButton contestant =
    button [ onClick <| AddEdgic contestant ] [ text "+" ]


addView : Model -> Html Msg
addView { selectedRating, selectedTone, selectedVisibility } =
    ul []
        [ li [] (text "Rating: " :: ratingButtons selectedRating)
        , li [] (text "Tone: " :: toneButtons selectedTone)
        , li [] (text "Visibility: " :: visibilityButtons selectedVisibility)
        ]


ratingButtons : Rating -> List (Html Msg)
ratingButtons selectedRating =
    [ button [ highlightStyle <| selectedRating == INV, onClick <| SelectRating INV ] [ text "INV" ]
    , button [ highlightStyle <| selectedRating == UTR, onClick <| SelectRating UTR ] [ text "UTR" ]
    , button [ highlightStyle <| selectedRating == MOR, onClick <| SelectRating MOR ] [ text "MOR" ]
    , button [ highlightStyle <| selectedRating == CP, onClick <| SelectRating CP ] [ text "CP" ]
    , button [ highlightStyle <| selectedRating == OTT, onClick <| SelectRating OTT ] [ text "OTT" ]
    ]


toneButtons : Tone -> List (Html Msg)
toneButtons selectedTone =
    [ button [ highlightStyle <| selectedTone == NN, onClick <| SelectTone NN ] [ text "NN" ]
    , button [ highlightStyle <| selectedTone == N, onClick <| SelectTone N ] [ text "N" ]
    , button [ highlightStyle <| selectedTone == M, onClick <| SelectTone M ] [ text "M" ]
    , button [ highlightStyle <| selectedTone == P, onClick <| SelectTone P ] [ text "P" ]
    , button [ highlightStyle <| selectedTone == PP, onClick <| SelectTone PP ] [ text "PP" ]
    ]


visibilityButtons : Visibility -> List (Html Msg)
visibilityButtons selectedVisibility =
    [ button [ highlightStyle <| selectedVisibility == One, onClick <| SelectVisibility One ] [ text "1" ]
    , button [ highlightStyle <| selectedVisibility == Two, onClick <| SelectVisibility Two ] [ text "2" ]
    , button [ highlightStyle <| selectedVisibility == Three, onClick <| SelectVisibility Three ] [ text "3" ]
    , button [ highlightStyle <| selectedVisibility == Four, onClick <| SelectVisibility Four ] [ text "4" ]
    , button [ highlightStyle <| selectedVisibility == Five, onClick <| SelectVisibility Five ] [ text "5" ]
    ]


highlightStyle : Bool -> Attribute msg
highlightStyle shouldHighlight =
    if shouldHighlight then
        style "font-weight" "bold"

    else
        style "" ""


ratingToString : Rating -> String
ratingToString rating =
    case rating of
        INV ->
            "INV"

        UTR ->
            "UTR"

        MOR ->
            "MOR"

        CP ->
            "CP"

        OTT ->
            "OTT"

        RatingUnselected ->
            ""


toneToString : Tone -> String
toneToString tone =
    case tone of
        NN ->
            "NN"

        N ->
            "N"

        M ->
            "M"

        P ->
            "P"

        PP ->
            "PP"

        ToneUnselected ->
            ""


visibilityToString : Visibility -> String
visibilityToString visibility =
    case visibility of
        One ->
            "1"

        Two ->
            "2"

        Three ->
            "3"

        Four ->
            "4"

        Five ->
            "5"

        VisibilityUnselected ->
            ""
