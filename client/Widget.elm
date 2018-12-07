import Browser
import Html exposing (Html, button, div, text, h1, h3, p, label, br, legend)
import Html.Attributes exposing (class, style)
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
          ( { model | formStatus = Opss }, Cmd.none )
        _ ->
          ({ model | form = Form.update validate formMsg form }, Cmd.none)

    SentForm result ->
      case result of
        Ok () ->
          ({model | formStatus = Sent}, Cmd.none)

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
        [ text ("Error of petition getting: " ++ (toString err))]
    Loading ->
      div []
        [ text "Loading petition" ]
    Loaded petition ->
      div
        [ style "margin" "50px 20px" 
        , style "width" "550px"   
        ]
        [ viewPetition petition
        , case formStatus of
            Ready -> Html.map FormMsg (formView form)
            None -> Html.map FormMsg (formView form)
            Sending -> text "Sending form..."
            Sent -> text "Congratuations! Your voice has been accounted"
            FormFailure err -> text ("Error of sending form: " ++ (toString err))
            Opss -> Html.map 
                      FormMsg 
                      (
                      div
                        []
                        [ p [] [text "Please, fill all required fields"]
                        ,formView form
                        ]
                      )
         ]

getFormErrorsString : Form () SignerForm -> List (Html Form.Msg)
getFormErrorsString form =
    List.map (\(a, b) -> text (a ++ ": " ++ (Debug.toString b))) (getErrors form)

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

viewPetition : Petition -> Html Msg
viewPetition petition = 
    div
      []
      [ h1 [class "display-1"] [text ("SIGN THE PETITION: " ++ petition.petitionName) ]
      , toHtml [class "display-3"] petition.petitionDescription
          -- h3 [class "display-3"] [text (petition.petitionDescription) ]
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
        |> andMap (field "email" (oneOf [emptyString, email]))
        |> andMap (field "phone" (string |> defaultValue ""))
        |> andMap (field "birth_year" (int |> andThen (minInt 1900)))
        |> andMap (field "gender" string |> defaultValue "M")
        |> andMap (field "notifies_enabled" bool)


formView : Form () SignerForm -> Html Form.Msg
formView form =
    let
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
        ]
        [ legend [] [ text "Petition form" ]    
        -- , div [] (getFormErrorsString form)
        , textGroup "First Name* " (Form.getFieldAsString "first_name" form)
        , textGroup "Last Name* " (Form.getFieldAsString "last_name" form)
        , textGroup "Country* " (Form.getFieldAsString "country" form)
        , textGroup "City* " (Form.getFieldAsString "city" form)
        , textGroup "Organization" (Form.getFieldAsString "organization" form)
        , textGroup "Email" (Form.getFieldAsString "email" form)
        , textGroup "Phone" (Form.getFieldAsString "phone" form)
        , textGroup "Birth Year* " (Form.getFieldAsString "birth_year" form)        
        , selectGroup genderOptions "Gender" (Form.getFieldAsString "gender" form)        
        , checkboxGroup "Notifies Enabled" (Form.getFieldAsBool "notifiesEnabled" form)        

        , formActions
            [ button
                [ onClick Form.Submit
                , class "btn btn-primary"
                ]
                [ text "Submit" ]
            , text " "
            , button
                [ onClick (Form.Reset [])
                , class "btn btn-default"
                ]
                [ text "Reset" ]
            ]

        -- , button
        --     [ onClick Form.Submit ]
        --     [ text "Submit" ]
        ]
