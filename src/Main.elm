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
  , secure : Bool
  , contentTypes : List ContentType
  }

type alias Model =
  { agents : List Agent
  , errors : List Http.Error
  }


initData : List Agent
initData =
  [ Agent "Agent 1" "localhost:8081" False []
  , Agent "Agent 2" "localhost:8082" False []
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
          -- Debug.log (Debug.toString agentContentTypes)
          ({ model | agents = applyAgentContentTypesResponse agentContentTypes model.agents}, Cmd.none)

        Err error ->
          -- Debug.log "Err error"
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
getTotalContentTypes agents =
    -- Debug.log (Debug.toString (Set.toList (Set.fromList (List.concatMap extractContentTypes agents))))
    Set.fromList (List.concatMap extractContentTypes agents)

toTableHeader : Agent -> Html msg
toTableHeader a = th [] [ text a.name ]

agentHasContentType : ContentType -> Agent -> Html msg
agentHasContentType contentType agent =
  if List.member contentType agent.contentTypes then
    td [] [ text "yes" ]
  else
    td [] [ text "no" ]

toTableRow : List Agent -> ContentType -> Html msg
toTableRow agents contentType =
  tr []
  (
    [td [] [ text contentType ]]
    ++ List.map (agentHasContentType contentType) agents
  )

viewContentTypes : Set ContentType -> List Agent -> Html div
viewContentTypes totalContentTypes agents =
  div []
    [ table [ Html.Attributes.attribute "border" "1" ]
      (
        [ thead []
          (
            [ th [] [ text "Content Types" ] ]
            ++ List.map toTableHeader agents
          )
        ]
        ++ List.map (toTableRow agents) (Set.toList totalContentTypes)
      )
    ]

view : Model -> Html Msg
view model =
  -- Debug.log ("[VIEW] Model=>" ++ (Debug.toString model))
  div []
    [ h2 [] [ text "Agent Landing Page" ]
    , viewContentTypes (getTotalContentTypes model.agents) model.agents
    ]


-- HTTP

getAgentContentTypes : Agent -> Cmd Msg
getAgentContentTypes agent =
  let
    agent_url =
      if agent.secure then
        "https://" ++ agent.host ++ "/api/v1/types"
      else
        "http://" ++ agent.host ++ "/api/v1/types"
  in
    Http.get
      { url = agent_url
      , expect = Http.expectJson AgentContentTypes (agentContentTypesDecoder agent)
      }

agentContentTypesDecoder : Agent -> Decoder AgentContentTypesResponse
agentContentTypesDecoder agent =
  -- Debug.log "in decoder"
  map2 AgentContentTypesResponse
    (Json.Decode.succeed agent.name)
    (field "content_types" (Json.Decode.list Json.Decode.string))
