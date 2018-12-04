import Browser
import Html exposing (Html, button, div, text, h1, p, label, br, legend)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onClick)
import Generated.Api exposing (Petition, SignerForm, getPetitionByCode, postPetitionByCodeSigner)
import Http

import Form exposing (Form)
import Form.Input as Input
import Form.Validate as Validate exposing (..)
import View.Bootstrap exposing (..)
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
  | Ready
  | None

type alias Model = 
  {
     url            : String
    ,code           : String
    ,locale         : String
    ,petitionStatus : PetitionStatus
    ,formStatus     : FormStatus
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
    , form = Form.initial [] validate }
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
  = GotPetition (Result Http.Error Petition)
  | SentForm (Result Http.Error ())
  | NoOp
  | FormMsg Form.Msg

update : Msg -> Model -> (Model, Cmd Msg)
update msg ({ url, code, locale, form } as model) =
  let signer0 = { 
                    signerFormFirstName  = "a"
                  , signerFormLastName = "b"
                  , signerFormCountry = "c"
                  , signerFormCity = "c"
                  , signerFormOrganization = "d"
                  , signerFormEmail = "e"
                  , signerFormPhone = "f"
                  , signerFormBirthYear = 22
                  , signerFormGender = "M"
                  , signerFormNotifiesEnabled = False
                }
  in
  case msg of
    GotPetition result ->
      case result of
        Ok petition ->
          ({model | petitionStatus = Loaded petition, formStatus = Ready}, Cmd.none)

        Err err ->
          ({model | petitionStatus = PetitionFailure err}, Cmd.none)

    FormMsg formMsg ->
      case ( formMsg, Form.getOutput form ) of
        ( Form.Submit, Just signer ) ->
          (
            { model | formStatus = Sending }, 
            Http.send SentForm (postPetitionByCodeSigner url code signer)
          )
        ( Form.Submit, Nothing ) ->
          (
            { model | formStatus = Sending }, 
            Http.send SentForm (postPetitionByCodeSigner url code signer0)
          )
        _ ->
          ({ model | form = Form.update validate formMsg form }, Cmd.none)

    SentForm result ->
      case result of
        Ok () ->
          ({model | formStatus = Ready}, Cmd.none)

        Err err ->
          ({model | formStatus = FormFailure err}, Cmd.none)

    NoOp ->
        (model, Cmd.none)


-- VIEW

view : Model -> Html Msg
view {url, code, locale, petitionStatus, formStatus, form} =
  case petitionStatus of
    PetitionFailure err ->
      div []
        [ text ("Ошибка получения петиции: " ++ (toString err))]
    Loading ->
      div []
        [ text "Загрузка петиции" ]
    Loaded petition ->
      div
        [ style "margin" "50px 20px" 
        , style "width" "550px"   
        ]
        [ 
          h1 [] [text (petition.petitionName) ],
          p  [] [text (petition.petitionDescription) ],
          Html.map FormMsg (formView form)
        ]

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none
 
toString err =
  case err of
    Http.BadUrl url              -> "Плохой url: " ++ url
    Http.Timeout                 -> "Вышло время"
    Http.NetworkError            -> "Ошибка сети"
    Http.BadStatus response      -> "Плохой статус: " ++ response.status.message
    Http.BadPayload str response -> "Битый формат ответа: " ++ str

validate : Validation () SignerForm
validate = 
    succeed SignerForm
        |> andMap (field "first_name" string)
        |> andMap (field "last_name" string)
        |> andMap (field "country" string)
        |> andMap (field "city" string)
        |> andMap (field "organization" string)
        |> andMap (field "email" string)
        |> andMap (field "phone" string)
        |> andMap (field "birth_year" int)
        |> andMap (field "gender" string)
        |> andMap (field "notifies_enabled" bool)


formView : Form () SignerForm -> Html Form.Msg
formView form =
    let
        -- error presenter
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

        genderOptions = [("M", "Male"), ("F", "Female")]
            
    in
      div
        [ class "form-horizontal"
        -- , style "margin" "50px 20px" 
        -- , style "width" "550px"   
        ]
        [ legend [] [ text "Elm Simple Form example" ]    

        , textGroup "First Name" (Form.getFieldAsString "first_name" form)        
        , textGroup "Last Name" (Form.getFieldAsString "last_name" form)        
        , textGroup "Country" (Form.getFieldAsString "country" form)        
        , textGroup "City" (Form.getFieldAsString "city" form)        
        , textGroup "Organization" (Form.getFieldAsString "organization" form)        
        , textGroup "Email" (Form.getFieldAsString "email" form)        
        , textGroup "Phone" (Form.getFieldAsString "phone" form)        
        , textGroup "Birth Year" (Form.getFieldAsString "birth_year" form)        
        , selectGroup genderOptions "Gender" (Form.getFieldAsString "gender" form)        
        , checkboxGroup "Notifies Enabled" (Form.getFieldAsBool "notifiesEnabled" form)        

        , button
            [ onClick Form.Submit ]
            [ text "Submit" ]
        ]
