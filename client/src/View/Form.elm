module View.Form exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Form exposing (Form, FieldState)
import Form.Input as Input
import Form.Error exposing (Error, ErrorValue)
import String exposing (fromInt)
-- import Model exposing (..)




type alias GroupBuilder a =
    -- String -> FieldState CustomError a -> Html Form.Msg
    String -> FieldState () a -> Html Form.Msg


-- formGroup : String -> Maybe (Error ()) -> List (Html Form.Msg) -> Html Form.Msg
-- formGroup : Bool -> String -> Maybe (ErrorValue ()) -> List (Html Form.Msg) -> Html Form.Msg
-- formGroup hidden labelQ maybeError inputs =
--     case hidden of
--         True ->
--             div [ class "hidden" ] inputs
--         False ->
--             div inputs
--     div
--         [ case hidden of
--             True  -> class "hidden"
--             False -> class ("row form-group " ++ (errorClass maybeError) ) ]
--         [ colQ 3 False
--             [ label [ class "control-label" ] [ text labelQ ] ]
--         , colQ 5 False
--             inputs
--         , errorMessage maybeError
--         ]


formActions : List (Html Form.Msg) -> Html Form.Msg
formActions content =
    div [ class "controls" ] content 


textGroup : GroupBuilder String
textGroup labelQ state =
    Input.textInput state
            [ placeholder labelQ
            , value (Maybe.withDefault "" state.value)
            -- , class (errorClass state.liveError) 
            , class (errorClass state.error) 
            ]

textGroupHidden : GroupBuilder String
textGroupHidden labelQ state =
    Input.textInput state
            [ placeholder labelQ
            , value (Maybe.withDefault "" state.value)
            , class "hidden"
            ]


checkboxGroup : GroupBuilder Bool
checkboxGroup labelQ state =
    label 
        [ class "checkbox" ]
        [ Input.checkboxInput state []
        , div [ class "checkbox-label" ] [ text labelQ ]
        ]


selectGroup : List ( String, String ) -> GroupBuilder String
selectGroup options labelQ state =
    div 
        [ class "select-div" ]
        [ Input.selectInput options state
            [ placeholder labelQ
            , value (Maybe.withDefault "" state.value)
            , class (errorClass state.liveError) 
            ]
        ]


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
-- errorMessage : Maybe (ErrorValue ()) -> Html Form.Msg
-- errorMessage maybeError =
--     case maybeError of
--         Just error ->
--             colQ 
--                 4 
--                 False
--                 [ p 
--                     [ class "help-block" ] 
--                     [ text ("Error: " ++ (Debug.toString error)) ]
--                 ]

--         Nothing ->
--             colQ 
--                 4 
--                 True
--                 [ span
--                     [ class "help-block" ]
--                     -- [ text "\x2007" ]
--                     [ text "\u{2007}" ]
--                 ]
