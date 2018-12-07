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


colQ : Int -> Bool -> List (Html Form.Msg) -> Html Form.Msg
colQ i hidden content =
    let h = case hidden of
            True -> "hidden"
            False -> ""
    in
    div [ class ("col-sm-" ++ fromInt i ++ " col-xs-12 " ++ h) ] content


type alias GroupBuilder a =
    -- String -> FieldState CustomError a -> Html Form.Msg
    String -> FieldState () a -> Html Form.Msg


-- formGroup : String -> Maybe (Error ()) -> List (Html Form.Msg) -> Html Form.Msg
formGroup : Bool -> String -> Maybe (ErrorValue ()) -> List (Html Form.Msg) -> Html Form.Msg
formGroup hidden labelQ maybeError inputs =
    div
        [ case hidden of
            True  -> class "hidden"
            False -> class ("row form-group " ++ (errorClass maybeError) ) ]
        [ colQ 3 False
            [ label [ class "control-label" ] [ text labelQ ] ]
        , colQ 5 False
            inputs
        , errorMessage maybeError
        ]


formActions : List (Html Form.Msg) -> Html Form.Msg
formActions content =
    row
        [ div [ class "col-xs-offset-3 col-xs-9" ] content ]


textGroup : GroupBuilder String
textGroup labelQ state =
    formGroup False labelQ
        state.liveError
        -- state.error
        [ Input.textInput state
            [ class "form-control"
            , value (Maybe.withDefault "" state.value)
            ]
        ]

textGroupHidden : GroupBuilder String
textGroupHidden labelQ state =
    formGroup True labelQ
        state.liveError
        -- state.error
        [ Input.textInput state
            [ class "form-control hidden"
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
    formGroup False ""
        state.liveError
        -- state.error
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
    formGroup False labelQ
        state.liveError
        -- state.error
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


-- errorMessage : Maybe (Error ()) -> Html Form.Msg
errorMessage : Maybe (ErrorValue ()) -> Html Form.Msg
errorMessage maybeError =
    case maybeError of
        Just error ->
            colQ 
                4 
                False
                [ p 
                    [ class "help-block" ] 
                    [ text ("Error: " ++ (Debug.toString error)) ]
                ]

        Nothing ->
            colQ 
                4 
                True
                [ span
                    [ class "help-block" ]
                    -- [ text "\x2007" ]
                    [ text "\u{2007}" ]
                ]
