import Browser
import Html exposing (Html, button, div, text, h1, h2, h3, h5, a, p, label, br, legend, span, img, ul, li, i)
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

import Widget.Common as Common

main =
  Browser.element { init = init, update = update, subscriptions = subscriptions, view = view }


-- MODEL

type CurrentWindow
  = DescriptionW
  | FormW

type alias Model = 
  {
    common              : Common.Model
  , currentWindow       : CurrentWindow
  }

init : {url: String, code: String, locale: String} -> (Model, Cmd Msg)
init ({url, code, locale} as params) =
  ( { common = Common.initModel params
    , currentWindow = DescriptionW
    }
  , Http.send (CommonMsg << Common.GotPetition) (getPetitionByCode url code (Common.prepareLocale locale))
  )

-- UPDATE

type Msg
  = CommonMsg Common.Msg
  | NextWindow

update : Msg -> Model -> (Model, Cmd Msg)
-- update msg ({ url, code, locale, form, currentWindow } as model) =
update msg ({ common, currentWindow } as model) =
  let mm = Common.m model.common.locale
  in
  case msg of
    CommonMsg msgC -> 
      let (newCommon, cmd) = Common.update msgC model.common
      in
      ( { model | common = newCommon }, Cmd.map CommonMsg cmd)

    NextWindow -> 
      let w = if currentWindow == DescriptionW then FormW else DescriptionW
      in 
        ({model | currentWindow = w}, Cmd.none)

-- VIEW

view : Model -> Html Msg
view { common, currentWindow} =
  let mm = m common.locale
      mmC = m common.locale << CommonTextMessage
  in
  case common.petitionStatus of
    -- FIXME: Повторы в default, slider...
    Common.PetitionFailure err ->
      div []
        [ text ("Error of petition getting: " ++ (Common.errorToString err))]
    Common.Loading ->
      div []
        [ text "Loading petition" ]
    Common.Loaded pModel ->
      let
        (mainContent, footerContent) =
          case currentWindow of
            DescriptionW -> 
              ( viewPetition common.url common.code common.locale pModel.petition pModel.signersCount 
              , [ 
                  div
                  [ class "read-more"]
                  [ a
                      [ target "_blank"
                      , class "btn btn-primary"
                      , href (common.url ++ "/petitionText.html/" ++ common.code ++ "?locale=" ++ common.locale)
                      ]
                      [ text (mmC Common.ShowFullTextMsg)] 
                  ]
                , div 
                    [ class "next-window"
                    , onClick NextWindow 
                    ]
                    [ text (mm FillFormFooterMsg) ]
                ]
              )
            FormW ->
              ( case common.formStatus of
                  Common.Sending -> text "Sending form..."
                  Common.FormFailure err -> text ("Error of sending form: " ++ (Common.errorToString err))
                  -- _ -> div [] [ Html.map FormMsg (formView locale flash form) ]
                  _ -> Html.map (CommonMsg << Common.FormMsg) (Common.formView common.locale common.flash common.formData) 
              , [ div 
                    [ class "next-window"
                    , onClick NextWindow 
                    ]
                    [ text (mm ShowPetitionFooterMsg) ]
                ]
              )
      in
      div
        [ id "petition-content" ]
        [ div 
            [ id "petition-content-header" ]
            [ h1 [] [ text (pModel.petition.petitionName) ] ]
        , mainContent
        , div [ id "petition-content-footer" ] footerContent
        ]

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

viewPetition : String -> String -> String -> Petition -> (Maybe Int) -> Html Msg
viewPetition url code locale petition cnt = 
    let mm = m locale
        mmC = m locale << CommonTextMessage
        cntQ = withDefault "?" (map fromInt cnt)
    in
    div
      [id "petition-info"]
      [ img [src (url ++ Common.petitionsImagesPath ++ petition.petitionCode ++ ".png")] []
      , div 
        [ id "petition-info-signed"]
        [ div [ id "petition-info-signed-div1"] [text (mmC Common.WasSignedMsg)] 
        , div [ id "petition-info-signed-div2" ] [ text cntQ ] 
        ]
      , div 
        [ id "petition-info-description" 
        ]
        [ toHtml [] petition.petitionDescription 
        ]

      ]


type TextMessage 
  = CommonTextMessage Common.TextMessage
  | FillFormFooterMsg
  | ShowPetitionFooterMsg


m : String -> TextMessage -> String
m locale msg =
  case locale of
    "ru" -> 
      case msg of
        CommonTextMessage msgC -> Common.m locale msgC
        FillFormFooterMsg -> "Подписать петицию"
        ShowPetitionFooterMsg -> "Текст петиции"
    _ ->
      case msg of
        CommonTextMessage msgC -> Common.m locale msgC
        FillFormFooterMsg -> "Sign petition"
        ShowPetitionFooterMsg -> "Text of petition"

