import Browser
import Html exposing (Html, button, div, text, h1, h2, h3, h5, a, p, label, br, legend, span)
import Html.Attributes exposing (class, style, type_, attribute, id, tabindex, href, target)
import Html.Events exposing (onClick)
import Generated.Api exposing (Petition, SignerForm, getPetitionByCode, postPetitionByCodeSigner)
import Http
import List as List exposing (map)

import Form exposing (Form, getErrors)
import Form.Input as Input
import Form.Field as Field
import Form.Validate as Validate exposing (..)
import View.Bootstrap exposing (..)
import Markdown exposing (..)
import String exposing (toInt, fromInt)
import Platform.Cmd as Cmd

import Widget.Common exposing (..)


main =
  Browser.element { init = init, update = update, subscriptions = subscriptions, view = view }


-- MODEL

init : {url: String, code: String, locale: String} -> (Model, Cmd Msg)
init ({url, code, locale} as params) =
  ( initModel params
  , Http.send GotPetition (getPetitionByCode url code (prepareLocale locale))
  )

-- VIEW

view : Model -> Html Msg
view model =
  let mm = m model.locale
  in
  case model.petitionStatus of
    PetitionFailure err ->
      div []
        [ text ("Error of petition getting: " ++ (errorToString err))]
    Loading ->
      div []
        [ text "Loading petition" ]
    Loaded pModel ->
      div
        [ style "margin" "50px 20px" 
        , style "width" "90%"   
        ]
        [ viewPetition model.url model.code model.locale pModel.petition 
        , case model.formStatus of
            Sending -> text "Sending form..."
            FormFailure err -> text ("Error of sending form: " ++ (errorToString err))
            _ -> 
                div
                    []
                    [ viewSignersCount model.locale pModel.signersCount
                    , Html.map FormMsg (formView model.locale model.formData)
                    ]
        ]

viewSignersCount : String -> (Maybe Int) -> Html Msg
viewSignersCount locale cntQ =
  let mm = m locale
  in
  case cntQ of 
    (Just cnt) -> 
      div
        []
        [ br [] []
        , p
            [ class "alert alert-info" ]
            [ text ((mm WasSignedMsg) ++ (fromInt cnt) ++ (mm PeopleMsg)) ]
        ]
    Nothing -> div [] []

getFormErrorsString : Form () SignerForm -> List (Html Form.Msg)
getFormErrorsString form =
    List.map (\(a, b) -> text (a ++ ": " ++ (Debug.toString b))) (getErrors form)

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

viewPetition : String -> String -> String -> Petition -> Html Msg
viewPetition url code locale petition = 
    let mm = m locale
    in
    div
      []
      -- [ h1 [class "display-2"] [text ("SIGN THE PETITION: " ++ petition.petitionName) ]
      [ h2 [] [text ((mm SignPetitionMsg) ++ petition.petitionName) ]
      -- , toHtml [class "display-3"] petition.petitionDescription
      , toHtml [] petition.petitionDescription
      , a
          [ target "_blank"
          , class "btn btn-primary"
          , href (url ++ "/petitionText.html/" ++ code ++ "?locale=" ++ locale)
          ]
          [ text (mm ShowFullTextMsg)] 
      -- , button 
      --     [ type_ "button"
      --     , class "btn btn-primary"
      --     , attribute "data-toggle" "modal"
      --     , attribute "data-target" "#petition-content"
      --     ]
      --     [ text (mm ShowFullTextMsg)] 
      , br [] []

      , div 
        [ class "modal"
        , id "petition-content"
        , tabindex (-1)
        , attribute "role" "dialog"
        , attribute "aria-labelledby" "petition-content-title"
        , attribute "aria-hidden" "true"
        ]
        [ div 
          [ class "modal-dialog modal-dialog-centered"
          , attribute "role" "document"
          ]
          [ div 
            [ class "modal-content" ]
            [ div
                [ class "modal-header" ]
                [ h5 
                    [ class "modal-title"
                    , id "petition-content-title"
                    ]
                    [text ((mm SignPetitionMsg) ++ petition.petitionName)]
                , button
                    [ type_ "button"
                    , class "close"
                    , attribute "data-dismiss" "modal"
                    , attribute "aria-label" "close"
                    ]
                    [ span [attribute "aria-hidden" "true"] [text "X"] ]
                ]
            , div [class "modal-body"] [ toHtml [] petition.petitionContent]
            , div 
              [ class "modal-footer" ]
              [ button
                  [ type_ "button"
                  , class "btn btn-secondary"
                  , attribute "data-dismiss" "modal"
                  ]
                  [ text (mm CloseMsg)]
              ]
            ]
          ]
        ]
      ]
 
formView : String -> Form () SignerForm -> Html Form.Msg
formView locale form =
    let
        mm = m locale
        errorFor field =
            case field.liveError of
                Just error ->
                    -- replace toString with your own translations
                    div [ class "error" ] [ text (Debug.toString error) ]

                Nothing ->
                    text ""

        -- fields states
        firstName =
            Form.getFieldAsString "first_name" form

        lastName =
            Form.getFieldAsString "last_name" form

        country =
            Form.getFieldAsString "country" form

        city =
            Form.getFieldAsString "city" form

        organization =
            Form.getFieldAsString "organization" form

        email =
            Form.getFieldAsString "email" form

        phone =
            Form.getFieldAsString "phone" form

        birthYear =
            Form.getFieldAsString "birth_year" form

        gender =
            Form.getFieldAsString "gender" form

        notifiesEnabled =
            Form.getFieldAsBool "notifies_enabled" form

        genderOptions = [("M", (mm MaleMsg)), ("F", (mm FemaleMsg))]
            
    in
      div
        [ class "form-horizontal"
        ]
        [ legend [] [ text (mm PetitionFormMsg) ]    
        -- , div [] (getFormErrorsString form)
        , textGroup (mm FirstNameMsg) (Form.getFieldAsString "first_name" form)
        , textGroup (mm LastNameMsg) (Form.getFieldAsString "last_name" form)
        , textGroup (mm CountryMsg) (Form.getFieldAsString "country" form)
        , textGroup (mm CityMsg) (Form.getFieldAsString "city" form)
        , textGroup (mm OrganizationMsg) (Form.getFieldAsString "organization" form)
        , textGroup (mm EmailPhoneMsg) (Form.getFieldAsString "email" form)
        , textGroupHidden (mm PhoneMsg) (Form.getFieldAsString "phone" form)
        , textGroupHidden (mm BirthYearMsg) (Form.getFieldAsString "birth_year" form)        
        , selectGroup genderOptions (mm GenderMsg) (Form.getFieldAsString "gender" form)        
        , checkboxGroup (mm KeepMeUpdateMsg) (Form.getFieldAsBool "notifies_enabled" form)        

        , formActions
            [ button
                [ onClick Form.Submit
                , class "btn btn-primary"
                ]
                [ text (mm SubmitMsg) ]
            , text " "
            , button
                [ onClick (Form.Reset [])
                , class "btn btn-default"
                ]
                [ text (mm ResetMsg) ]
            ]
        ]

