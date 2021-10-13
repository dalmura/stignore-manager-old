-- stignore frontend for stignore-agent(s)
--
-- See https://github.com/dalmura/stignore-manager for more details
--
module Main exposing (..)

import Browser
import Set exposing (Set)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode exposing (Decoder, field, string, list, map2)



-- MAIN

main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }



-- MODEL

type alias ContentType = String

type alias AgentContentTypesResponse =
  { name : String
  , contentTypes : List ContentType
  }

type alias Agent =
  { name : String
  , host : String
  , contentTypes : List ContentType
  }

type alias Model =
  { agents : List Agent
  , errors : List Http.Error
  }


initData : List Agent
initData =
  [ Agent "Agent 1" "localhost:8081" []
  , Agent "Agent 2" "localhost:8082" []
  ]



init : () -> (Model, Cmd Msg)
init _ =
  ( Model initData []
  , Cmd.batch (List.map getAgentContentTypes initData)
  )


-- UPDATE

type Msg
  = AddAgent Agent
  | AgentContentTypes (Result Http.Error AgentContentTypesResponse)


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    AddAgent agent ->
      ({ model | agents = model.agents ++ [agent] }, getAgentContentTypes agent)

    AgentContentTypes result ->
      case result of
        Ok agentContentTypes ->
          ({model | agents = applyAgentContentTypesResponse agentContentTypes model.agents}, Cmd.none)

        Err error ->
          ({ model | errors = model.errors ++ [error] }, Cmd.none)


applyAgentContentTypesResponse : AgentContentTypesResponse -> List Agent -> List Agent
applyAgentContentTypesResponse response agents =
  let
    inspectAndSet agent =
      if response.name == agent.name then
        { agent | contentTypes = response.contentTypes }
      else
        agent
  in
    List.map inspectAndSet agents


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none


-- VIEW

extractContentTypes : Agent -> List ContentType
extractContentTypes a = a.contentTypes

getTotalContentTypes : List Agent -> Set ContentType
getTotalContentTypes agents = Set.fromList (List.concatMap extractContentTypes agents)

toTableHeader : Agent -> Html msg
toTableHeader a = th [] [ text a.name ]

viewContentTypes : Set ContentType -> List Agent -> Html div
viewContentTypes totalContentTypes agents =
  div []
    [ table []
      ([ thead []
        ([ th [] [ text "Content Types" ] ]
         ++ List.map toTableHeader agents
        )
      ])
    ]

view : Model -> Html Msg
view model =
  div []
    [ h2 [] [ text "Agent Landing Page" ]
    , viewContentTypes (getTotalContentTypes model.agents) model.agents
    ]


-- HTTP

getAgentContentTypes : Agent -> Cmd Msg
getAgentContentTypes agent =
  let
    agent_url = "https://" ++ agent.host ++ "/api/v1/types"
  in
    Http.get
      { url = agent_url
      , expect = Http.expectJson AgentContentTypes agentContentTypesDecoder
      }

agentContentTypesDecoder : Decoder AgentContentTypesResponse
agentContentTypesDecoder =
  map2 AgentContentTypesResponse
    (field "data" (field "name" string))
    (field "data" (field "contentTypes" (Json.Decode.list Json.Decode.string)))
