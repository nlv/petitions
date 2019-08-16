module Generated.Api exposing (..)

import Json.Decode
import Json.Encode exposing (Value)
-- The following module comes from bartavelle/json-helpers
import Json.Helpers exposing (..)
import Dict exposing (Dict)
import Set
import Http
import String
import Url.Builder

maybeBoolToIntStr : Maybe Bool -> String
maybeBoolToIntStr mx =
  case mx of
    Nothing -> ""
    Just True -> "1"
    Just False -> "0"


type alias Petition' =
    { _petitionId : Int
    , _petitionCode : String
    , _petitionName : String
    , _petitionDescription : String
    , _petitionContent : String
    , _petitionLocale : String
    }

decodePetition' : Decoder Petition'
decodePetition' =
    decode Petition
        |> required "_petitionId" int
        |> required "_petitionCode" string
        |> required "_petitionName" string
        |> required "_petitionDescription" string
        |> required "_petitionContent" string
        |> required "_petitionLocale" string

type alias SignerForm =
    { _signerFormFirstName : String
    , _signerFormLastName : String
    , _signerFormCountry : String
    , _signerFormCity : String
    , _signerFormOrganization : String
    , _signerFormEmail : String
    , _signerFormPhone : String
    , _signerFormBirthYear : Int
    , _signerFormGender : String
    , _signerFormNotifiesEnabled : Bool
    }

decodeSignerForm : Decoder SignerForm
decodeSignerForm =
    decode SignerForm
        |> required "_signerFormFirstName" string
        |> required "_signerFormLastName" string
        |> required "_signerFormCountry" string
        |> required "_signerFormCity" string
        |> required "_signerFormOrganization" string
        |> required "_signerFormEmail" string
        |> required "_signerFormPhone" string
        |> required "_signerFormBirthYear" int
        |> required "_signerFormGender" string
        |> required "_signerFormNotifiesEnabled" bool

encodeSignerForm : SignerForm -> Json.Encode.Value
encodeSignerForm x =
    Json.Encode.object
        [ ( "_signerFormFirstName", Json.Encode.string x._signerFormFirstName )
        , ( "_signerFormLastName", Json.Encode.string x._signerFormLastName )
        , ( "_signerFormCountry", Json.Encode.string x._signerFormCountry )
        , ( "_signerFormCity", Json.Encode.string x._signerFormCity )
        , ( "_signerFormOrganization", Json.Encode.string x._signerFormOrganization )
        , ( "_signerFormEmail", Json.Encode.string x._signerFormEmail )
        , ( "_signerFormPhone", Json.Encode.string x._signerFormPhone )
        , ( "_signerFormBirthYear", Json.Encode.int x._signerFormBirthYear )
        , ( "_signerFormGender", Json.Encode.string x._signerFormGender )
        , ( "_signerFormNotifiesEnabled", Json.Encode.bool x._signerFormNotifiesEnabled )
        ]

getPetitionByCode : String -> (Maybe String) -> (Result Http.Error  (((Petition' Int String String String String String), Int))  -> msg) -> Cmd msg
getPetitionByCode capture_code query_locale toMsg =
    let
        params =
            List.filterMap identity
            (List.concat
                [ [ query_locale
                    |> Maybe.map (Url.Builder.string "locale") ]
                ])
    in
        Http.request
            { method =
                "GET"
            , headers =
                []
            , url =
                Url.Builder.crossOrigin ""
                    [ "petition"
                    , capture_code
                    ]
                    params
            , body =
                Http.emptyBody
            , expect =
                Http.expectJson toMsg jsonDec((Petition' Int Text Text Text Text Text), Int)
            , timeout =
                Nothing
            , tracker =
                Nothing
            }



postPetitionByCodeSigner : String -> SignerForm -> (Result Http.Error  (Int)  -> msg) -> Cmd msg
postPetitionByCodeSigner capture_code body toMsg =
    let
        params =
            List.filterMap identity
            (List.concat
                [])
    in
        Http.request
            { method =
                "POST"
            , headers =
                []
            , url =
                Url.Builder.crossOrigin ""
                    [ "petition"
                    , capture_code
                    , "signer"
                    ]
                    params
            , body =
                Http.jsonBody (jsonEncSignerForm body)
            , expect =
                Http.expectJson toMsg Json.Decode.int
            , timeout =
                Nothing
            , tracker =
                Nothing
            }