module STIgnore.Listing exposing (Listing, KVListing, Item, new, raw, name, ignoreType, toTuple, fromTuple, pretty, decoder, encode, toString, toKV)

--import ContentTypes exposing (ContentType)

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required)
import Json.Encode as Encode exposing (Value)

import Dict exposing (Dict)


-- TYPES


type alias Listing = List Item
type alias KVListing = Dict String Item


type alias Item =
    { raw : String
    , name : String
    , ignoreType : String
    }



-- INFO


new : String -> String -> String -> Item
new newRaw newName newIgnoreType =
    Item newRaw newName newIgnoreType


raw : Item -> String
raw item =
    item.raw


name : Item -> String
name item =
    item.name


ignoreType : Item -> String
ignoreType item =
    item.ignoreType


toTuple : Item -> (String, String, String)
toTuple item =
    (item.raw, item.name, item.ignoreType)


fromTuple : (String, String, String) -> Item
fromTuple (newRaw, newName, newIgnoreType) =
    Item newRaw newName newIgnoreType


pretty : Item -> String
pretty item =
    item.name ++ " (" ++ item.ignoreType ++ ")"


toString : Listing -> String
toString listing =
    List.map pretty listing
        |> String.join ", "


toKV : Listing -> KVListing
toKV listing =
    let
        toAssoc : Item -> (String, Item)
        toAssoc item =
            (item.name, item)
    in
    List.map toAssoc listing
        |> Dict.fromList


-- SERIALIZATION


itemDecoder : Decoder Item
itemDecoder =
    Decode.succeed Item
        |> required "raw" Decode.string
        |> required "name" Decode.string
        |> required "ignore_type" Decode.string


decoder : Decoder Listing
decoder =
    (Decode.list itemDecoder) 


itemEncoder : Item -> Value
itemEncoder item =
    Encode.object
        [ ( "raw", Encode.string item.raw )
        , ( "name", Encode.string item.name )
        , ( "ignore_type", Encode.string item.ignoreType )
        ]

encode : Listing -> Value
encode listing =
    Encode.list itemEncoder listing
