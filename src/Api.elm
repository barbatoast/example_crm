module Api exposing
    ( Firm
    , Lawyer
    , LawyerDetail
    , PagedFirms
    , PagedLawyers
    , getFirms
    , getLawyers
    , getLawyerDetail
    , postFirm
    )

import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode

-- MODEL

type alias Firm =
    { id : String
    , name : String
    , createdAt : String
    }

type alias PagedFirms =
    { firms : List Firm
    , totalPages : Int
    }

type alias Lawyer =
    { id : String
    , name : String
    , email : String
    }

firmDecoder : Decode.Decoder Firm
firmDecoder =
    Decode.map3 Firm
        (Decode.field "id" Decode.string)
        (Decode.field "name" Decode.string)
        (Decode.field "createdAt" Decode.string)

pagedFirmsDecoder : Decode.Decoder PagedFirms
pagedFirmsDecoder =
    Decode.map2 PagedFirms
        (Decode.field "firms" (Decode.list firmDecoder))
        (Decode.field "totalPages" Decode.int)

-- API CALL

getFirms : Int -> Int -> (Result Http.Error PagedFirms -> msg) -> Cmd msg
getFirms page limit toMsg =
    let
        url =
            "http://127.0.0.1:5000/firms?page=" ++ String.fromInt page ++ "&limit=" ++ String.fromInt limit
    in
    Http.get
        { url = url
        , expect = Http.expectJson toMsg pagedFirmsDecoder
        }

-- Lawyers

lawyerDecoder : Decoder Lawyer
lawyerDecoder =
    Decode.map3 Lawyer
        (Decode.field "id" Decode.string)
        (Decode.field "name" Decode.string)
        (Decode.field "email" Decode.string)

type alias PagedLawyers =
    { users : List Lawyer
    , totalPages : Int
    }

pagedLawyersDecoder : Decoder PagedLawyers
pagedLawyersDecoder =
    Decode.map2 PagedLawyers
        (Decode.field "users" (Decode.list lawyerDecoder))
        (Decode.field "totalPages" Decode.int)

getLawyers : Int -> Int -> (Result Http.Error PagedLawyers -> msg) -> Cmd msg
getLawyers page limit toMsg =
    Http.get
        { url = "http://127.0.0.1:5000/users?page=" ++ String.fromInt page ++ "&limit=" ++ String.fromInt limit
        , expect = Http.expectJson toMsg pagedLawyersDecoder
        }

-- Post Firm

postFirm : String -> (Result Http.Error Firm -> msg) -> Cmd msg
postFirm name toMsg =
    let
        body = Http.jsonBody <| Encode.object [ ( "name", Encode.string name ) ]
    in
    Http.post
        { url = "http://127.0.0.1:5000/firm/add"
        , body = body
        , expect = Http.expectJson toMsg firmDecoder
        }

-- Lawyer Detail

type alias LawyerDetail =
    { name : String
    , email : String
    , isAppUser : Bool
    }

lawyerDetailDecoder : Decode.Decoder LawyerDetail
lawyerDetailDecoder =
    Decode.map3 LawyerDetail
        (Decode.field "name" Decode.string)
        (Decode.field "email" Decode.string)
        (Decode.field "isAppUser" Decode.bool)

getLawyerDetail : String -> (Result Http.Error LawyerDetail -> msg) -> Cmd msg
getLawyerDetail id toMsg =
    Http.get
        { url = "http://127.0.0.1:5000/users/" ++ id
        , expect = Http.expectJson toMsg lawyerDetailDecoder
        }
