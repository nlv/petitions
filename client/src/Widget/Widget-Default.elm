import Browser
-- import Time exposing (now)
-- import Task exposing (perform)
import Html exposing (Html, button, div, text, h1, h2, h3, h5, a, p, label, br, legend, span, img)
import Html.Attributes exposing (class, style, type_, attribute, id, tabindex, href, target, src)
import Html.Events exposing (onClick)
import Generated.Api exposing (Petition, SignerForm, getPetitionByCode, postPetitionByCodeSigner)
import Http
import List as List exposing (map)

import Form exposing (Form, getErrors)
import Form.Input as Input
import Form.Field as Field
import Form.Validate as Validate exposing (..)
import View.Form exposing (..)
import Flash exposing (..)
import Markdown exposing (..)
import String exposing (toInt, fromInt)
import Maybe exposing (map, withDefault)
import Platform.Cmd as Cmd
-- import Html.Events exposing (onClick)

import Widget.Common as Common

main =
  Browser.element { init = init, update = update, subscriptions = subscriptions, view = view }


-- MODEL

-- initFormValues =
--   [("gender", Field.string "M"), ("notifies_enabled", Field.bool True)]

petitionsImagesPath = "/static/images/petitions/"

type alias Model = 
  {
    common              : Common.Model
  , descriptionExpanded : Bool
  }

init : {url: String, code: String, locale: String} -> (Model, Cmd Msg)
init ({url, code, locale} as params) =
  ( { common = Common.initModel params
    , descriptionExpanded = True
    }
  , Http.send (CommonMsg << Common.GotPetition) (getPetitionByCode url code (Common.prepareLocale locale))
  )


-- UPDATE

type Msg
  = CommonMsg Common.Msg
  | ToggleDescription
  | ExpandDescription
  | CollapseDescription

update : Msg -> Model -> (Model, Cmd Msg)
-- update msg ({ common, descriptionExpanded } as model) =
update msg model =
  let mm = Common.m model.common.locale
  in
  case msg of
    CommonMsg msgC -> 
      let (newCommon, cmd) = Common.update msgC model.common
      in
      ( { model | common = newCommon }, Cmd.map CommonMsg cmd)

    ToggleDescription -> ({model | descriptionExpanded = not model.descriptionExpanded}, Cmd.none)

    ExpandDescription -> ({model | descriptionExpanded = True}, Cmd.none)

    CollapseDescription -> ({model | descriptionExpanded = False}, Cmd.none)

-- VIEW

view : Model -> Html Msg
view { common, descriptionExpanded} =
  let mm = Common.m common.locale
  in
  case common.petitionStatus of
    Common.PetitionFailure err ->
      div []
        [ text ("Error of petition getting: " ++ (Common.errorToString err))]
    Common.Loading ->
      div []
        [ text "Loading petition" ]
    Common.Loaded pModel ->
      div
        []
        [ viewPetition common.url common.code common.locale pModel.petition descriptionExpanded pModel.signersCount
        , case common.formStatus of
            Common.Sending -> text "Sending form..."
            Common.FormFailure err -> text ("Error of sending form: " ++ (Common.errorToString err))
            _ -> 
                div [ onClick CollapseDescription ] [ Html.map (CommonMsg << Common.FormMsg) (formView common.locale common.flash common.formData) ]
        ]

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

viewPetition : String -> String -> String -> Petition -> Bool -> (Maybe Int) -> Html Msg
viewPetition url code locale petition descriptionExpanded cnt = 
    let mm = Common.m locale
        cntQ = withDefault "?" (map fromInt cnt)
    in
    div
      [id "petition-info"]
      [ h1 [] [text (petition.petitionName) ]
      , img [src (url ++ petitionsImagesPath ++ petition.petitionCode ++ ".png")] []
      , div 
        [ id "petition-info-signed"]
        [ div [ id "petition-info-signed-div1"] [text (mm Common.WasSignedMsg)] 
        , div [ id "petition-info-signed-div2" ] [ text cntQ ] 
        ]
      , div 
        [ id "petition-info-description" 
        , class (if descriptionExpanded then "expanded" else "collapsed")
        -- , onClick ExpandDescription
        , onClick ToggleDescription
        ]
        [ toHtml [] petition.petitionDescription 
        , div
          [ class "read-more"]
          [ a
              [ target "_blank"
              , class "btn btn-primary"
              , href (url ++ "/petitionText.html/" ++ code ++ "?locale=" ++ locale)
              ]
              [ text (mm Common.ShowFullTextMsg)] 
          ]
        ]
      , div 
          [ id "petition-info-desription-toggle" 
          , onClick ToggleDescription
          ]
          [ text (if descriptionExpanded then "\u{25B2}" else "\u{25BC}") ]
          -- ]
      -- , div [ id "petition-info-separator" ] []

      ]

formView : String -> Flash.State -> Form () SignerForm -> Html Form.Msg
formView locale flash form =
    let
        mm = Common.m locale
        errorFor field =
            case field.liveError of
                Just error ->
                    -- replace toString with your own translations
                    div [ class "error" ] [ text (Debug.toString error) ]

                Nothing ->
                    text ""

        -- fields states
        firstName = Form.getFieldAsString "first_name" form
        lastName = Form.getFieldAsString "last_name" form
        country = Form.getFieldAsString "country" form
        city = Form.getFieldAsString "city" form
        organization = Form.getFieldAsString "organization" form
        email = Form.getFieldAsString "email" form
        phone = Form.getFieldAsString "phone" form
        birthYear = Form.getFieldAsString "birth_year" form
        gender = Form.getFieldAsString "gender" form
        notifiesEnabled = Form.getFieldAsBool "notifies_enabled" form
        genderOptions = [("M", (mm Common.MaleMsg)), ("F", (mm Common.FemaleMsg))]

        (title, titleClass) = 
          Maybe.map (\x -> (x, "alert")) (Flash.getMessage flash) |> Maybe.withDefault (mm Common.PetitionFormMsg, "")
            
    in
      div
        [ id "petition-form" ]
        [ div 
            [ id "petition-form-title" ] 
            [ h1 [class titleClass] [ text title ] ]
        , div
            [ id "petition-form-content"]
            [ textGroup (mm Common.FirstNameMsg) (Form.getFieldAsString "first_name" form)
            , textGroup (mm Common.LastNameMsg) (Form.getFieldAsString "last_name" form)
            , textGroup (mm Common.CountryMsg) (Form.getFieldAsString "country" form)
            , textGroup (mm Common.CityMsg) (Form.getFieldAsString "city" form)
            , textGroup (mm Common.OrganizationMsg) (Form.getFieldAsString "organization" form)
            , textGroup (mm Common.EmailPhoneMsg) (Form.getFieldAsString "email" form)
            , textGroupHidden (mm Common.PhoneMsg) (Form.getFieldAsString "phone" form)
            , textGroupHidden (mm Common.BirthYearMsg) (Form.getFieldAsString "birth_year" form)        
            , selectGroup genderOptions (mm Common.GenderMsg) (Form.getFieldAsString "gender" form)        
            , checkboxGroup (mm Common.KeepMeUpdateMsg) (Form.getFieldAsBool "notifies_enabled" form)        
            , formActions
                [ button
                    [ onClick Form.Submit
                    ]
                    [ text (mm Common.SubmitMsg) ]
                , button
                    [ onClick (Form.Reset Common.initFormValues)
                    ]
                    [ text (mm Common.ResetMsg) ]
                ]
            ]
        ]
        -- [ class "form-horizontal"
        -- ]
        -- [ legend [] [ text (mm PetitionFormMsg) ]    
        -- -- , div [] (getFormErrorsString form)

