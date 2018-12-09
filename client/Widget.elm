import Browser
import Html exposing (Html, button, div, text, h1, h2, h3, h5, p, label, br, legend, span)
import Html.Attributes exposing (class, style, type_, attribute, id, tabindex)
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
-- import Html.Events exposing (onClick)


main =
  Browser.element { init = init, update = update, subscriptions = subscriptions, view = view }


-- MODEL

type PetitionStatus
  = PetitionFailure Http.Error
  | Loading
  | Loaded Petition

type FormStatus
  = FormFailure Http.Error
  | Sending
  | Sent
  | Ready
  | None
  | Opss

type alias Model = 
  {
     url            : String
    ,code           : String
    ,locale         : String
    ,petitionStatus : PetitionStatus
    ,formStatus     : FormStatus
    ,signersCount   : Maybe Int
    ,form           : Form () SignerForm 
  }

init : {url: String, code: String, locale: String} -> (Model, Cmd Msg)
init {url, code, locale} =
  ( {
      url = url
    , code = code
    , locale = locale
    , petitionStatus = Loading
    , formStatus = None
    -- , form = Form.initial [("gender", Field.string "M")] validate }
    , form = Form.initial [] validate 
    , signersCount = Nothing
    }
  , Http.send GotPetition (getPetitionByCode url code (prepareLocale locale))
  )

prepareLocale : String -> Maybe String
prepareLocale locale
  = case locale of
      "default" -> Nothing
      l         -> Just l


-- getFromServer : String -> Http.Request a -> Http.Request a
-- getFromServer server request =
--   { request | url = server ++ request.url }

-- UPDATE

type Msg
  = GotPetition (Result Http.Error (Petition, Int))
  | SentForm (Result Http.Error Int)
  | NoOp
  | FormMsg Form.Msg

update : Msg -> Model -> (Model, Cmd Msg)
update msg ({ url, code, locale, form } as model) =
  case msg of
    GotPetition result ->
      case result of
        Ok (petition, cnt) ->
          ( {model | petitionStatus = Loaded petition, signersCount = Just cnt, formStatus = Ready}
          , Cmd.none
          )

        Err err ->
          ({model | petitionStatus = PetitionFailure err, signersCount = Nothing}, Cmd.none)

    FormMsg formMsg ->
      case ( formMsg, Form.getOutput form ) of
        ( Form.Submit, Just signer ) ->
          (
            { model | formStatus = Sending }, 
            Http.send SentForm (postPetitionByCodeSigner url code signer)
          )
        ( Form.Submit, Nothing ) ->
          ( { model | formStatus = Opss }, Cmd.none )
        _ ->
          ({ model | form = Form.update validate formMsg form }, Cmd.none)

    SentForm result ->
      case result of
        Ok cnt ->
          ({model | formStatus = Sent, signersCount = Just cnt}, Cmd.none)

        Err err ->
          ({model | formStatus = FormFailure err}, Cmd.none)

    NoOp ->
        (model, Cmd.none)

-- VIEW

view : Model -> Html Msg
view {url, code, locale, petitionStatus, signersCount, formStatus, form} =
  let mm = m locale
  in
  case petitionStatus of
    PetitionFailure err ->
      div []
        [ text ("Error of petition getting: " ++ (toString err))]
    Loading ->
      div []
        [ text "Loading petition" ]
    Loaded petition ->
      div
        [ style "margin" "50px 20px" 
        , style "width" "90%"   
        ]
        [ viewPetition locale petition 
        , case formStatus of
            Ready -> 
                div
                    []
                    [ viewSignersCount locale signersCount
                    , Html.map FormMsg (formView locale form)
                    ]
            None -> 
                div
                    []
                    [ viewSignersCount locale signersCount
                    , Html.map FormMsg (formView locale form)
                    ]
            Sending -> text "Sending form..."
            Sent -> div
                      []
                      [ br [] []
                      , p
                          [ class "alert alert-success" ]
                          [ text (mm ThankYouMsg) ]
                      , viewSignersCount locale signersCount
                      ]
            FormFailure err -> text ("Error of sending form: " ++ (toString err))
            Opss -> Html.map 
                      FormMsg 
                      (
                      div
                        []
                        [ p [] [text (mm FillRequiredFieldsMsg)]
                        ,formView locale form
                        ]
                      )
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

viewPetition : String -> Petition -> Html Msg
viewPetition locale petition = 
    let mm = m locale
    in
    div
      []
      -- [ h1 [class "display-2"] [text ("SIGN THE PETITION: " ++ petition.petitionName) ]
      [ h2 [] [text ((mm SignPetitionMsg) ++ petition.petitionName) ]
      -- , toHtml [class "display-3"] petition.petitionDescription
      , toHtml [] petition.petitionDescription
      , button 
          [ type_ "button"
          , class "btn btn-primary"
          , attribute "data-toggle" "modal"
          , attribute "data-target" "#petition-content"
          ]
          [ text (mm ShowFullTextMsg)] 
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
 
toString err =
  case err of
    Http.BadUrl url              -> "Bad url: " ++ url
    Http.Timeout                 -> "Timeout"
    Http.NetworkError            -> "Network error"
    Http.BadStatus response      -> "Bad response status: " ++ response.status.message
    Http.BadPayload str response -> "Bad response format: " ++ str

validate : Validation () SignerForm
validate = 
    succeed SignerForm
        |> andMap (field "first_name" (string |> andThen nonEmpty))
        |> andMap (field "last_name" (string |> andThen nonEmpty))
        |> andMap (field "country" (string |> andThen nonEmpty))
        |> andMap (field "city" (string |> andThen nonEmpty))
        |> andMap (field "organization" string |> defaultValue "")
        |> andMap (field "email" (string |> andThen nonEmpty))
        -- |> andMap (field "email" (oneOf [emptyString, email]))
        |> andMap (field "phone" (string |> defaultValue ""))
        |> andMap (field "birth_year" (int |> defaultValue 1990))
        -- |> andMap (field "birth_year" (int |> andThen (minInt 1900) |> andThen (maxInt 1999)))
        |> andMap (field "gender" string |> defaultValue "M")
        |> andMap (field "notifies_enabled" bool)


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
        , checkboxGroup (mm KeepMeUpdateMsg) (Form.getFieldAsBool "notifiesEnabled" form)        

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

type TextMessage 
  = PetitionFormMsg
  | ShowFullTextMsg
  | FirstNameMsg 
  | LastNameMsg
  | CountryMsg
  | CityMsg
  | OrganizationMsg
  | EmailPhoneMsg
  | PhoneMsg
  | BirthYearMsg
  | GenderMsg
  | MaleMsg
  | FemaleMsg 
  | KeepMeUpdateMsg  
  | SubmitMsg
  | ResetMsg
  | ThankYouMsg
  | FillRequiredFieldsMsg
  | WasSignedMsg
  | PeopleMsg
  | SignPetitionMsg
  | CloseMsg

m : String -> TextMessage -> String
m locale msg =
  case locale of
    "ru" -> 
      case msg of 
        PetitionFormMsg -> "Укажите Ваши данные"
        ShowFullTextMsg -> "Показать полный текст"
        FirstNameMsg  -> "Имя*"
        LastNameMsg -> "Фамилия*"
        CountryMsg -> "Страна*"
        CityMsg -> "Город*"
        OrganizationMsg -> "Организация"
        EmailPhoneMsg -> "Эл. почта/Телефон*"
        PhoneMsg -> "Телефон"
        BirthYearMsg -> "Год рождения"
        GenderMsg -> "Пол*"
        MaleMsg -> "Мужской"
        FemaleMsg -> "Женский"
        KeepMeUpdateMsg -> "Информировать меня о петиции и других событиях"
        SubmitMsg -> "Отправить"
        ResetMsg -> "Очистить"
        ThankYouMsg -> "Благодарим Вас! Ваш голос учтен!"
        FillRequiredFieldsMsg -> "Заполните обязательные поля."
        WasSignedMsg -> "Петицию подисали "
        PeopleMsg -> " человек"
        SignPetitionMsg -> "ПОДПИШИТЕ ПЕТИЦИЮ: "
        CloseMsg -> "Закрыть"
    _ -> 
      case msg of 
        PetitionFormMsg -> "Petition Form"
        ShowFullTextMsg -> "Show full text"
        FirstNameMsg  -> "First Name*"
        LastNameMsg -> "Last Name*"
        CountryMsg -> "Country*"
        CityMsg -> "City*"
        OrganizationMsg -> "Organization"
        EmailPhoneMsg -> "Email/Phone*"
        PhoneMsg -> "Phone*"
        BirthYearMsg -> "BirthYear"
        GenderMsg -> "Gender*"
        MaleMsg -> "Male"
        FemaleMsg -> "Female"
        KeepMeUpdateMsg -> "Keep me updated via-email on this petition and related issues. "
        SubmitMsg -> "Submit"
        ResetMsg -> "Reset"
        ThankYouMsg -> "Thank you! Your vote was taken into account!"
        FillRequiredFieldsMsg -> "Please, fill all required fields"
        WasSignedMsg -> "The petition was signed by "
        PeopleMsg -> " people"
        SignPetitionMsg -> "SIGN THE PETITION: "
        CloseMsg -> "Close"
