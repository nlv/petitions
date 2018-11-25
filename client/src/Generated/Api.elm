module Generated.Api exposing (..)

import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Json.Encode
import Http
import String
import Url


type alias Petition =
    { petitionId : Int
    , petitionName : String
    , petitionShortDescription : String
    , petitionDescription : String
    , petitionLocale : String
    }

decodePetition : Decoder Petition
decodePetition =
    succeed Petition
        |> required "petitionId" int
        |> required "petitionName" string
        |> required "petitionShortDescription" string
        |> required "petitionDescription" string
        |> required "petitionLocale" string

getPetitionByCode : String -> Maybe (String) -> Http.Request (Petition)
getPetitionByCode capture_code query_locale =
    let
        params =
            List.filter (not << String.isEmpty)
                [ query_locale
                    |> Maybe.map (identity >> Url.percentEncode >> (++) "locale=")
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
                    [ "http://localhost:3000"
                    , "petition"
                    , capture_code |> Url.percentEncode
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