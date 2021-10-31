module Agents exposing (Agent(..), Agents, new, name, host, pretty, decoder, encode)

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required)
import Json.Encode as Encode exposing (Value)



-- TYPES


type Agent
    = Agent Internals


type alias Internals =
    { name : String
    , host : String
    }


type alias Agents = List Agent


new : String -> String -> Agent
new newName newHost =
    Agent (Internals newName newHost)


-- INFO


name : Agent -> String
name (Agent info) =
    info.name


host : Agent -> String
host (Agent info) =
    info.host


pretty : Agent -> String
pretty (Agent info) =
    info.name ++ " (" ++ info.host ++ ")"



-- SERIALIZATION


decoder : Decoder Agent
decoder =
    Decode.succeed Internals
        |> required "name" Decode.string
        |> required "host" Decode.string
        |> Decode.map Agent


encode : Agent -> Value
encode (Agent info) =
    Encode.object
        [ ( "name", Encode.string info.name )
        , ( "host", Encode.string info.host )
        ]
