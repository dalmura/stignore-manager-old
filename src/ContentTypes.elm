module ContentTypes exposing (ContentType(..), ContentTypes, new, name, pretty, decoder, encode, toString)

import Http

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required, hardcoded)
import Json.Encode as Encode exposing (Value)



-- TYPES


type ContentType
    = ContentType Internals


type alias Internals =
    { name : String
    }


type alias ContentTypes = List ContentType


new : String -> ContentType
new newName =
    ContentType (Internals newName)


-- INFO


name : ContentType -> String
name (ContentType info) =
    info.name


pretty : ContentType -> String
pretty (ContentType info) =
    info.name


toString : ContentType -> String
toString contentType =
    pretty contentType



-- SERIALIZATION


decoder : Decoder ContentType
decoder =
    Decode.succeed Internals
        |> required "name" Decode.string
        |> Decode.map ContentType


encode : ContentType -> Value
encode (ContentType info) =
    Encode.object
        [ ( "name", Encode.string info.name )
        ]
