module FlushItems exposing (Listing, Item, new, path, pretty, decoder, encoder, toString)

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required)
import Json.Encode as Encode exposing (Value)

import Dict exposing (Dict)
import Round



-- TYPES


type FlushAction
    = Delete


type alias Item =
    { path : String
    , size_megabytes : Float
    , action : FlushAction
    }


type alias Listing = List Item



-- INFO


new : String -> Float -> FlushAction -> Item
new newPath newSize newAction =
    Item newPath newSize newAction


path : Item -> String
path item =
    item.path


toHumanSize : Float -> String
toHumanSize sizeInMB =
    if sizeInMB > 1000 then
        (Round.round 2 (sizeInMB / 1000)) ++ " GB"
    else
        (Round.round 2 (sizeInMB)) ++ " MB"


pretty : Item -> String
pretty item =
    item.path ++ " (" ++ (toHumanSize item.size_megabytes) ++ ")"


toString : Listing -> String
toString listing =
    List.map pretty listing
        |> String.join ", "



-- SERIALIZATION

flushActionDecoder : Decoder FlushAction
flushActionDecoder =
    Decode.string |>
        Decode.andThen
            (\str ->
              case str of
                "delete" -> Decode.succeed Delete
                _ -> Decode.fail "Invalid Flush Action"
            )


itemDecoder : Decoder Item
itemDecoder =
    Decode.succeed Item
        |> required "path" Decode.string
        |> required "size_megabytes" Decode.float
        |> required "action" flushActionDecoder


decoder : Decoder Listing
decoder =
    (Decode.list itemDecoder) 


flushActionEncoder : FlushAction -> String
flushActionEncoder action =
    case action of
        Delete ->
            "delete"


itemEncoder : Item -> Value
itemEncoder item =
    Encode.object
        [ ( "path", Encode.string item.path )
        , ( "size_megabytes", Encode.float item.size_megabytes )
        , ( "action", Encode.string (flushActionEncoder item.action) )
        ]

encoder : Listing -> Value
encoder listing =
    Encode.list itemEncoder listing
