module View.Bootstrap exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Form exposing (Form, FieldState)
import Form.Input as Input
import Form.Error exposing (Error, ErrorValue)
import String exposing (fromInt)
-- import Model exposing (..)


row : List (Html Form.Msg) -> Html Form.Msg
row content =
    div [ class "row" ] content


colQ : Int -> List (Html Form.Msg) -> Html Form.Msg
colQ i content =
    div [ class ("col-xs-" ++ fromInt i) ] content


type alias GroupBuilder a =
    -- String -> FieldState CustomError a -> Html Form.Msg
    String -> FieldState () a -> Html Form.Msg


-- formGroup : String -> Maybe (Error CustomError) -> List (Html Form.Msg) -> Html Form.Msg
formGroup : String -> Maybe (ErrorValue ()) -> List (Html Form.Msg) -> Html Form.Msg
formGroup labelQ maybeError inputs =
    div
        [ class ("row form-group " ++ (errorClass maybeError) ) ]
        [ colQ 3
            [ label [ class "control-label" ] [ text labelQ ] ]
        , colQ 5
            inputs
        , colQ 4
            [ errorMessage maybeError ]
        ]


formActions : List (Html Form.Msg) -> Html Form.Msg
formActions content =
    row
        [ div [ class "col-xs-offset-3 col-xs-9" ] content ]


textGroup : GroupBuilder String
textGroup labelQ state =
    formGroup labelQ
        state.liveError
        [ Input.textInput state
            [ class "form-control"
            , value (Maybe.withDefault "" state.value)
            ]
        ]


-- textAreaGroup : GroupBuilder String
-- textAreaGroup labelQ state =
--     formGroup labelQ
--         state.liveError
--         [ Input.textArea state
--             [ class "form-control"
--             , value (Maybe.withDefault "" state.value)
--             ]
--         ]


checkboxGroup : GroupBuilder Bool
checkboxGroup labelQ state =
    formGroup ""
        state.liveError
        [ div
            [ class "checkbox" ]
            [ label []
                [ Input.checkboxInput state []
                , text labelQ
                ]
            ]
        ]


selectGroup : List ( String, String ) -> GroupBuilder String
selectGroup options labelQ state =
    formGroup labelQ
        state.liveError
        [ Input.selectInput options state [ class "form-control" ] ]


-- radioGroup : List ( String, String ) -> GroupBuilder String
-- radioGroup options labelQ state =
--     let
--         item ( v, l ) =
--             label
--                 [ class "radio-inline" ]
--                 [ Input.radioInput v state []
--                 , text l
--                 ]
--     in
--         formGroup labelQ
--             state.liveError
--             (List.map item options)


errorClass : Maybe error -> String
errorClass maybeError =
    Maybe.map (\_ -> "has-error") maybeError |> Maybe.withDefault ""


-- errorMessage : Maybe (Error CustomError) -> Html Form.Msg
errorMessage : Maybe (ErrorValue ()) -> Html Form.Msg
errorMessage maybeError =
    case maybeError of
        Just error ->
            p
                [ class "help-block" ]
                -- [ text (fromInt error) ]
                [ text (Debug.toString error) ]

        Nothing ->
            span
                [ class "help-block" ]
                -- [ text "\x2007" ]
                [ text "\u{2007}" ]
