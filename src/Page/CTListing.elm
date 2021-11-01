module Page.CTListing exposing (Model, Msg, init, subscriptions, toSession, update, view)

{-| The Content Types Listing page.
-}

import Api exposing (Cred)
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode exposing (Decoder, decodeString, field, string)
import Json.Decode.Pipeline exposing (optional)
import Json.Encode as Encode
import Route exposing (Route)
import Session exposing (Session)
import Viewer exposing (Viewer)

import Agents exposing (Agents, Agent)
import ContentTypes exposing (ContentTypes, ContentType)
import ContentTypes.Slug exposing (Slug)
import ContentTypes.Listing as Listing exposing (Listing, Item)

import Dict exposing (Dict)
import Set exposing (Set)


-- MODEL


type alias Model =
    { session : Session
    , contentType : ContentType
    , listings : Dict String Listing
    }



init : Session -> Slug -> ( Model, Cmd Msg )
init session slug =
    let
        agents =
            Session.agents session

        contentType =
            ContentTypes.fromSlug slug

        agentListing : ContentType -> Agent -> Cmd Msg
        agentListing ctype agent =
            Api.listing (Session.cred session) agent ctype
                |> Http.send GotAgentListing
    in
    ( { session = session
      , contentType = contentType
      , listings = Dict.empty
      }
    , Cmd.batch (List.map (agentListing contentType) agents)
    )



-- VIEW


view : Model -> { title : String, content : Html Msg }
view model =
    { title = "Listing"
    , content =
        div [ class "listing-page" ]
            [ div [ class "container page" ]
                [ div [ class "row" ]
                    [ div [ class "col-md-12" ] <|
                        case (Dict.isEmpty model.listings) of
                            True ->
                                [ text "No Agents had this Content Type?" ]
                            False ->
                                agentListingsTable (Session.agents model.session) model.listings model.contentType
                    ]
                ]
            ]
    }


agentListingsTable : Agents -> Dict String Listing -> ContentType -> List (Html Msg)
agentListingsTable agents listings ctype =
    let
        saveUnique : String -> Listing -> Set (String, Int) -> Set (String, Int)
        saveUnique _ listing accum =
            Set.fromList (List.map Listing.toTuple listing)
                |> Set.union accum

        uniqueItems = Dict.foldl saveUnique Set.empty listings
            |> Set.toList
            |> List.sort
            |> List.map Listing.fromTuple

        toTableHeader : Agent -> Html Msg
        toTableHeader agent = th [] [ text (Agents.name agent) ]

        agentHasItem : Dict String Listing -> Listing.Item -> String -> Html Msg
        agentHasItem lookup item key =
            case (Dict.get key lookup) of
                Just items ->
                    if List.member item items then
                        td [] [ text "yes" ]
                    else
                        td [] [ text "no" ]
                Nothing ->
                    td [] [ text "N/A" ]

        toTableRow : Agents -> Dict String Listing -> Item -> Html Msg
        toTableRow rowAgents lookup item =
            tr []
            (
                [ td [] [ text (Listing.name item) ]
                , td [] [ text (String.fromInt (Listing.size item)) ]
                ]
                ++ (
                    List.map Agents.pretty rowAgents
                        |> List.map (agentHasItem lookup item)
                )
            )
    in
    [ div []
        [ table [ class "listing-table" ]
            (
                [ thead []
                    (
                        [ th [] [ text "Item" ]
                        , th [] [ text "Size" ]
                        ]
                        ++ List.map toTableHeader agents
                    )
                ]
                ++ List.map (toTableRow agents listings) uniqueItems
            )
        ]
    ]



-- UPDATE


type Msg
    = GotAgentListing (Result Http.Error (Agent, Listing))
    | GotSession Session


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotAgentListing (Ok (agent, listing)) ->
            let
                agentStr = Agents.pretty agent

                maybeExisting = Dict.get agentStr model.listings

                newListings =
                    case maybeExisting of
                        Just current ->
                            Dict.insert agentStr (current ++ listing) model.listings

                        Nothing ->
                            Dict.insert agentStr listing model.listings
            in
            ( { model | listings = newListings }, Cmd.none )

        GotAgentListing (Err error) ->
            ( model, Cmd.none )

        GotSession session ->
            ( { model | session = session }
            , Route.replaceUrl (Session.navKey session) Route.Home
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Session.changes GotSession (Session.navKey model.session)



-- EXPORT


toSession : Model -> Session
toSession model =
    model.session
