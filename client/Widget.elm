import Browser
import Time exposing (now)
import Task exposing (perform)
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
-- import Html.Events exposing (onClick)


main =
  Browser.element { init = init, update = update, subscriptions = subscriptions, view = view }


-- MODEL

initFormValues =
  [("gender", Field.string "M"), ("notifies_enabled", Field.bool True)]

flashTimeout = 5000

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
    ,flash          : Flash.State
  }

init : {url: String, code: String, locale: String} -> (Model, Cmd Msg)
init {url, code, locale} =
  ( {
      url = url
    , code = code
    , locale = locale
    , petitionStatus = Loading
    , formStatus = None
    , form = Form.initial initFormValues validate 
    , signersCount = Nothing
    , flash = Flash.none
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
  | SetFlash String
  | RemoveFlash 

update : Msg -> Model -> (Model, Cmd Msg)
update msg ({ url, code, locale, form } as model) =
  let mm = m locale
  in
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
          ( { model | formStatus = Opss }
          , Task.perform (always (SetFlash (mm FillRequiredFieldsMsg))) now
          )
        _ ->
          ({ model | form = Form.update validate formMsg form }, Cmd.none)

    SentForm result ->
      case result of
        Ok cnt ->
          ( {model | formStatus = Sent, signersCount = Just cnt, form = Form.initial initFormValues validate}
          , Task.perform (always (SetFlash (mm ThankYouMsg))) now
          )

        Err err ->
          ({model | formStatus = FormFailure err}, Cmd.none)

    NoOp ->
        (model, Cmd.none)

    SetFlash flashMessage ->
      let
        ( message, cmd) = 
          Flash.setFlash RemoveFlash flashTimeout flashMessage
      in
        ({ model | flash = message }, cmd)    

    RemoveFlash ->
      ({ model | flash = Flash.none }, Cmd.none)        


-- VIEW

view : Model -> Html Msg
view {url, code, locale, petitionStatus, signersCount, formStatus, form, flash} =
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
        []
        [ viewPetition url code locale petition signersCount
        , case formStatus of
            Ready -> 
                div [] [ Html.map FormMsg (formView locale flash form) ]
            None -> 
                div [] [ Html.map FormMsg (formView locale flash form) ]
            Sending -> text "Sending form..."
            Sent -> 
                div [] [ Html.map FormMsg (formView locale flash form) ]
            FormFailure err -> text ("Error of sending form: " ++ (toString err))
            Opss -> 
                div [] [ Html.map FormMsg (formView locale flash form) ]
                -- Html.map 
                --           FormMsg 
                --           (
                --           div
                --             []
                --             [ p [] [text (mm FillRequiredFieldsMsg)]
                --             ,formView locale form
                --             ]
                --           )
        ]

getFormErrorsString : Form () SignerForm -> List (Html Form.Msg)
getFormErrorsString form =
    List.map (\(a, b) -> text (a ++ ": " ++ (Debug.toString b))) (getErrors form)

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

viewPetition : String -> String -> String -> Petition -> (Maybe Int) -> Html Msg
viewPetition url code locale petition cnt = 
    let mm = m locale
        cntQ = withDefault "?" (map fromInt cnt)
    in
    div
      [id "petition-info"]
      -- [ h [] [text ((mm SignPetitionMsg) ++ petition.petitionName) ]
      [ h1 [] [text (petition.petitionName) ]
      , img [src (url ++ "/static/petition.png")] []
      , div 
        [ id "petition-info-signed"]
        [ div [ id "petition-info-signed-div1"] [text (mm WasSignedMsg)] 
        , div [ id "petition-info-signed-div2" ] [ text cntQ ] 
        ]
      , div 
        [ id "petition-info-description" ]
        [ toHtml [] petition.petitionDescription 
        , p
          [ class "read-more"]
          [ a
              [ target "_blank"
              , class "btn btn-primary"
              , href (url ++ "/petitionText.html/" ++ code ++ "?locale=" ++ locale)
              ]
              [ text (mm ShowFullTextMsg)] 
          ]
        ]
      , div [ id "petition-info-separator" ] []

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


formView : String -> Flash.State -> Form () SignerForm -> Html Form.Msg
formView locale flash form =
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
        genderOptions = [("M", (mm MaleMsg)), ("F", (mm FemaleMsg))]

        (title, titleClass) = 
          Maybe.map (\x -> (x, "alert")) (Flash.getMessage flash) |> Maybe.withDefault (mm PetitionFormMsg, "")
            
    in
      div
        [ id "petition-form" ]
        -- [ h1 [] [ text (mm PetitionFormMsg) ]
        [ h1 [class titleClass] [ text title ]
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
                ]
                [ text (mm SubmitMsg) ]
            , button
                [ onClick (Form.Reset initFormValues)
                ]
                [ text (mm ResetMsg) ]
            ]
        ]
        -- [ class "form-horizontal"
        -- ]
        -- [ legend [] [ text (mm PetitionFormMsg) ]    
        -- -- , div [] (getFormErrorsString form)

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
        PetitionFormMsg -> "Заполните форму"
        ShowFullTextMsg -> "Читать полностью"
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
        WasSignedMsg -> "Подписало:"
        PeopleMsg -> " человек"
        SignPetitionMsg -> "ПОДПИШИТЕ ПЕТИЦИЮ: "
        CloseMsg -> "Закрыть"
    _ -> 
      case msg of 
        PetitionFormMsg -> "Fill the form"
        ShowFullTextMsg -> "Read full text"
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
        WasSignedMsg -> "Signed by:"
        PeopleMsg -> " people"
        SignPetitionMsg -> "SIGN THE PETITION: "
        CloseMsg -> "Close"
