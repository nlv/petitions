module Generated.Api exposing (..)

import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Json.Encode
import Http
import String
import Url


type alias Petition =
    { petitionId : Int
    , petitionCode : String
    , petitionName : String
    , petitionDescription : String
    , petitionLocale : String
    }

decodePetition : Decoder Petition
decodePetition =
    succeed Petition
        |> required "_petitionId" int
        |> required "_petitionCode" string
        |> required "_petitionName" string
        |> required "_petitionDescription" string
        |> required "_petitionLocale" string

type alias SignerForm =
    { signerFormFirstName : String
    , signerFormLastName : String
    , signerFormCountry : String
    , signerFormOrganization : String
    , signerFormEmail : String
    , signerFormPhone : String
    , signerFormBirthYear : Int
    , signerFormGender : String
    , signerFormNotifiesEnabled : Bool
    }

decodeSignerForm : Decoder SignerForm
decodeSignerForm =
    succeed SignerForm
        |> required "_signerFormFirstName" string
        |> required "_signerFormLastName" string
        |> required "_signerFormCountry" string
        |> required "_signerFormOrganization" string
        |> required "_signerFormEmail" string
        |> required "_signerFormPhone" string
        |> required "_signerFormBirthYear" int
        |> required "_signerFormGender" string
        |> required "_signerFormNotifiesEnabled" bool

encodeSignerForm : SignerForm -> Json.Encode.Value
encodeSignerForm x =
    Json.Encode.object
        [ ( "_signerFormFirstName", Json.Encode.string x.signerFormFirstName )
        , ( "_signerFormLastName", Json.Encode.string x.signerFormLastName )
        , ( "_signerFormCountry", Json.Encode.string x.signerFormCountry )
        , ( "_signerFormOrganization", Json.Encode.string x.signerFormOrganization )
        , ( "_signerFormEmail", Json.Encode.string x.signerFormEmail )
        , ( "_signerFormPhone", Json.Encode.string x.signerFormPhone )
        , ( "_signerFormBirthYear", Json.Encode.int x.signerFormBirthYear )
        , ( "_signerFormGender", Json.Encode.string x.signerFormGender )
        , ( "_signerFormNotifiesEnabled", Json.Encode.bool x.signerFormNotifiesEnabled )
        ]

getPetitionByCode : String -> String -> Maybe (String) -> Http.Request (Petition)
getPetitionByCode url capture_code query_locale =
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
                    [ url
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

postPetitionByCodeSigner : String -> String -> SignerForm -> Http.Request (())
postPetitionByCodeSigner url capture_code body =
    Http.request
        { method =
            "POST"
        , headers =
            -- [ Http.header  "Content-Type" "application/json"  ]
            []
        , url =
            String.join "/"
                [ url
                , "petition"
                , capture_code |> Url.percentEncode
                , "signer"
                ]
        , body =
            Http.jsonBody (encodeSignerForm body)
        , expect = Http.expectStringResponse (\_ -> Ok ())
            -- Http.expectStringResponse
            --     (\{body2} ->
            --         if String.isEmpty body2 then
            --             Ok ()
            --         else
            --             Err "Expected the response body to be empty"
            --     ) 
        , timeout =
            Nothing
        , withCredentials =
            False
        }