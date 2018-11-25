import Browser
import Html exposing (Html, button, div, text, h1, p)
import Generated.Api exposing (Petition, getPetitionByCode)
import Http
-- import Html.Events exposing (onClick)


main =
  Browser.element { init = init, update = update, subscriptions = subscriptions, view = view }


-- MODEL

type PetitionStatus
  = Failure Http.Error
  | Loading
  | Loaded Petition

type alias Model = 
  {
     url    : String
    ,code   : String
    ,locale : String
    ,status : PetitionStatus
  }

init : {url: String, code: String, locale: String} -> (Model, Cmd Msg)
init {url, code, locale} =
  ( {url = url, code = code, locale = locale, status = Loading }
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

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    GotPetition result ->
      case result of
        Ok petition ->
          ({model | status = Loaded petition}, Cmd.none)

        Err err ->
          ({model | status = Failure err}, Cmd.none)


-- VIEW

view : Model -> Html Msg
view {url, code, locale, status} =
  case status of
    Failure err ->
      div []
        [ text ("Ошибка получения петиции: " ++ (toString err))]
    Loading ->
      div []
        [ text "Загрузка петиции" ]
    Loaded petition ->
      div []
        [ 
          h1 [] [text (petition.petitionName) ],
          p  [] [text (petition.petitionShortDescription) ]
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