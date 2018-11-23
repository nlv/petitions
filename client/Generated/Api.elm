module Generated.Api exposing (..)

import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Json.Encode
import Http
import String


type alias Petition =
    { petitionId : Int
    , petitionName : String
    , petitionShortDescription : String
    , petitionDescription : String
    , petitionLocale : String
    }

decodePetition : Decoder Petition
decodePetition =
    decode Petition
        |> required "petitionId" int
        |> required "petitionName" string
        |> required "petitionShortDescription" string
        |> required "petitionDescription" string
        |> required "petitionLocale" string

getPetitionById : Int -> Maybe (String) -> Http.Request (Petition)
getPetitionById capture_id query_locale =
    let
        params =
            List.filter (not << String.isEmpty)
                [ query_locale
                    |> Maybe.map (Http.encodeUri >> (++) "locale=")
                    |> Maybe.withDefault ""
                ]
    in
        Http.request
            { method =
                "GET"
            , headers =
                []
            , url =
                String.join "/"
                    [ ""
                    , "petition"
                    , capture_id |> toString |> Http.encodeUri
                    ]
                ++ if List.isEmpty params then
                       ""
                   else
                       "?" ++ String.join "&" params
            , body =
                Http.emptyBody
            , expect =
                Http.expectJson decodePetition
            , timeout =
                Nothing
            , withCredentials =
                False
            }