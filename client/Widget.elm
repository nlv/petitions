import Browser
import Html exposing (Html, button, div, text, h1, p)
import Generated.Api exposing (Petition, getPetitionByCode)
import Http
-- import Html.Events exposing (onClick)


main =
  Browser.element { init = init, update = update, subscriptions = subscriptions, view = view }


-- MODEL

type Model
  = Failure Http.Error
  | Loading
  | Success Petition

init : () -> (Model, Cmd Msg)
init _ =
  ( Loading
  , Http.send GotPetition (getPetitionByCode "zerro" (Just "ru"))
  )

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
          (Success petition, Cmd.none)

        Err err ->
          (Failure err, Cmd.none)


-- VIEW

view : Model -> Html Msg
view model =
  case model of
    Failure err ->
      div []
        [ text ("Ошибка получения петиции: " ++ (toString err))]
    Loading ->
      div []
        [ text "Загрузка петиции" ]
    Success petition ->
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