module Page.CTListing exposing (Model, Msg, init, subscriptions, toSession, update, view)

{-| The Content Types Listing page.
-}

import Api exposing (Cred)
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Encode as Encode
import Route exposing (Route)
import Session exposing (Session)
import Viewer exposing (Viewer)

import Agents exposing (Agents, Agent)
import ContentTypes exposing (ContentTypes, ContentType)
import ContentTypes.Slug exposing (Slug)
import ContentTypes.Listing as CTListing
import STIgnore.Listing as STIListing
import STIActions exposing (STIActionClass(..), STIActionType(..), STIAction, STIActions, AgentSTIActions)
import Modal

import Dict exposing (Dict)
import Set exposing (Set)
import String.Extra

import Round



-- MODEL


type ItemStatus
    = Exists Float
    | IgnoredItem
    | MissingItem
    | MissingAgent


type alias Model =
    { session : Session
    , contentType : ContentType
    , ctListings : Dict String CTListing.KVListing
    , stiListings : Dict String STIListing.KVListing
    , nextStiId : Int
    , agentStiActions : AgentSTIActions
    , applyModalOpen : Bool
    , flushModalOpen : Bool
    }


init : Session -> Slug -> ( Model, Cmd Msg )
init session slug =
    let
        agents =
            Session.agents session

        contentType =
            ContentTypes.fromSlug slug

        agentCTListing : ContentType -> Agent -> Cmd Msg
        agentCTListing ctype agent =
            Api.ctListing (Session.cred session) agent ctype
                |> Http.send GotAgentCTListing

        agentSTIListing : ContentType -> Agent -> Cmd Msg
        agentSTIListing ctype agent =
            Api.stiListing (Session.cred session) agent ctype
                |> Http.send GotAgentSTIListing
    in
    ( { session = session
      , contentType = contentType
      , ctListings = Dict.empty
      , stiListings = Dict.empty
      , nextStiId = 1
      , agentStiActions = Dict.empty
      , applyModalOpen = False
      , flushModalOpen = False
      }
    , Cmd.batch (
        (List.map (agentCTListing contentType) agents)
        ++
        (List.map (agentSTIListing contentType) agents)
      )
    )



-- VIEW


view : Model -> { title : String, content : Html Msg }
view model =
    { title = "Listing"
    , content =
        div [ class "listing-page" ]
            (
                (
                    if model.applyModalOpen then
                        [(renderApplyModal model)]
                    else
                        []
                ) ++
                (
                    if model.flushModalOpen then
                        [(renderFlushModal model)]
                    else
                        []
                ) ++
                [ div [ class "container page" ]
                    [ div [ class "row" ]
                        [ div [ class "col-md-12" ] <|
                            summaryAndActionsTable model.contentType model.agentStiActions
                        ]
                    , div [ class "row" ]
                        [ div [ class "col-md-12" ] <|
                            case (Dict.isEmpty model.ctListings) of
                                True ->
                                    [ text "No Agents had this Content Type?" ]
                                False ->
                                    agentListingsTable (Session.agents model.session) model.ctListings model.stiListings model.contentType
                        ]
                    ]
                ]
            )
    }


renderApplyModal : Model -> Html Msg
renderApplyModal model =
    let
        agentKeys = Dict.keys model.agentStiActions

        agentStiActionsToTable : AgentSTIActions -> String -> Html msg
        agentStiActionsToTable agentStiActions agentKey =
            let
                maybeAgent = Agents.fromKey agentKey
                maybeStiActions = Dict.get agentKey agentStiActions
            in
                case (maybeAgent, maybeStiActions) of
                    (Just agent, Just stiActions) ->
                        div []
                            [ h3 [] [ text (Agents.name agent) ]
                            , pre [] [ text (Encode.encode 2 (STIActions.encoder stiActions)) ]
                            ]
                    (_, _) -> 
                        div [] []

        modalBody =
            [ div [] [ text "We're going to apply:" ]
            , div [] (List.map (agentStiActionsToTable model.agentStiActions) agentKeys)
            ]
    in
    Modal.new "apply-modal" "Apply Actions" modalBody CloseApplyModal

renderFlushModal : Model -> Html Msg
renderFlushModal model =
    let
        modalBody = [div [] [ text "We're going to flush:" ]]
    in
    Modal.new "flush-modal" "Flush Changes" modalBody CloseFlushModal


stiActionToTableRow : Agent -> STIAction -> Html Msg
stiActionToTableRow agent stiAction =
    let
        actionClass =
            case stiAction.actionClass of
                Add -> "Add"
                Remove -> "Remove"

        actionType =
            case stiAction.actionType of
                Ignore -> "Ignore"
                Keep -> "Keep"
    in
        tr []
        [ td [] [ text actionClass ]
        , td [] [ text (String.Extra.ellipsis 60 stiAction.name) ]
        , td [] [ text actionType ]
        , td [] [ a [ onClick (RemoveSTIAction agent stiAction.id)
                      , href ""
                      , class "tag-pill tag-default"
                    ] [ text "X" ]
                ]
        ]


stiActionsToTable : String -> STIActions -> Html Msg
stiActionsToTable agentKey stiActions =
    let
        maybeAgent = Agents.fromKey agentKey
    in
        case maybeAgent of
            (Just agent) ->
                if List.isEmpty stiActions then
                    div [] []
                else
                    div []
                        [ h1 [] [ text (Agents.name agent)]
                        , table [ class "stilisting-table" ]
                            (
                                [ thead []
                                    [ th [ style "text-align" "center" ] [ text "Action" ]
                                    , th [ style "text-align" "center" ] [ text "Item" ]
                                    , th [ style "text-align" "center" ] [ text "Type" ]
                                    , th [ style "text-align" "center" ] [ text "Remove" ]
                                    ]
                                ]
                                ++ List.map (stiActionToTableRow agent) stiActions
                            )
                        , br [] []
                        ]
            Nothing ->
                div [] [ text ("Unable to parse agent: " ++ agentKey) ]


summaryAndActionsTable : ContentType -> AgentSTIActions -> List (Html Msg)
summaryAndActionsTable ctype agentStiActions =
    let
        noStiActions =
            if Dict.isEmpty agentStiActions then
                True
            else if List.isEmpty (List.concat (Dict.values agentStiActions)) then
                True
            else
                False

        actionsTable =
            case noStiActions of
                True ->
                    []
                False ->
                    (
                        [ h4 []
                            [ text "Pending Actions"
                            , text " "
                            , button [ class "btn btn-sm btn-primary", onClick ApplySTIActions ] [ text "Apply Changes" ]
                            , text " "
                            , button [ class "btn btn-sm btn-primary", onClick FlushSTIActions ] [ text "Flush Changes" ]
                            ]
                        ]
                        ++ (Dict.map stiActionsToTable agentStiActions
                            |> Dict.values)
                        ++ [ br [] [] ]
                    )
    in
        (
            [ h2 [] [ text (String.Extra.toTitleCase (ContentTypes.name ctype)) ]
            , br [] []
            ] ++ actionsTable
        )


agentItemStatusToCell : String -> Float -> (Agent, ItemStatus) -> Html Msg
agentItemStatusToCell itemName targetSize (agent, itemStatus) =
    case itemStatus of
        Exists size ->
            if size == targetSize then
                td [ class "ctlisting-yes" ]
                    [ a [ onClick (AddSTIAction agent Add Ignore itemName)
                        , href ""
                        , class "tag-pill tag-default"
                        ] [ text "yes" ]
                    ]
            else
                td [ class "ctlisting-yes" ] [ text "yes (size conflict)" ]
        IgnoredItem ->
            td [ class "ctlisting-ignored" ]
                [ a [ onClick (AddSTIAction agent Remove Ignore itemName)
                    , href ""
                    , class "tag-pill tag-default"
                    ] [ text "ignored" ]
                ]
        MissingItem ->
            td [ class "ctlisting-no"] [ text "no" ]
        MissingAgent ->
            td [] [ text "N/A" ]


itemStatusSize : (Agent, ItemStatus) -> Maybe Float
itemStatusSize (agent, itemStatus) =
    case itemStatus of
        Exists size -> Just size
        _ -> Nothing


agentHasItem : Dict String CTListing.KVListing -> Dict String STIListing.KVListing -> String -> Agent -> (Agent, ItemStatus)
agentHasItem ctLookup stiLookup itemName agent =
    let
        agentName = Agents.pretty agent
    in
    case (Dict.get agentName ctLookup) of
        Just ctListing ->
            case (Dict.get itemName ctListing) of
                Just item ->
                    (agent, Exists (CTListing.size item))
                Nothing ->
                    case (Dict.get agentName stiLookup) of
                        Just stiListing ->
                            case (Dict.get itemName stiListing) of
                                Just item ->
                                    (agent, IgnoredItem)
                                Nothing ->
                                    (agent, MissingItem)
                        Nothing ->
                            (agent, MissingItem)
        Nothing ->
            (agent, MissingAgent)


toHumanSize : Float -> String
toHumanSize sizeInMB =
    if sizeInMB > 1000 then
        (Round.round 2 (sizeInMB / 1000)) ++ " GB"
    else
        (Round.round 2 (sizeInMB)) ++ " MB"


agentListingsTable : Agents -> Dict String CTListing.KVListing -> Dict String STIListing.KVListing -> ContentType -> List (Html Msg)
agentListingsTable agents ctListings stiListings ctype =
    let
        saveUnique : String -> CTListing.KVListing -> Set String -> Set String
        saveUnique _ listing accum =
            Dict.values listing
                |> List.map CTListing.name
                |> Set.fromList
                |> Set.union accum

        uniqueNames = Dict.foldl saveUnique Set.empty ctListings
            |> Set.toList
            |> List.sort

        toTableHeader : Agent -> Html Msg
        toTableHeader agent = th [] [ text (Agents.name agent) ]

        toTableRow : Agents -> Dict String CTListing.KVListing -> Dict String STIListing.KVListing -> String -> Html Msg
        toTableRow rowAgents ctLookup stiLookup itemName =
            let
                agentItemStatusRow = List.map (agentHasItem ctLookup stiLookup itemName) rowAgents
                rowSizes = List.filterMap itemStatusSize agentItemStatusRow

                replicationCount = List.length rowSizes
                replicationCountMin = 2

                maxSize = Maybe.withDefault 0 (List.maximum rowSizes)
                maxSizeHuman = toHumanSize maxSize

                itemNameEllipsis =
                    String.Extra.ellipsis 60 itemName

                itemNameCell =
                    if replicationCount < replicationCountMin then
                        td [ class "ctlisting-low-replication" ] [ text itemNameEllipsis ]
                    else
                        td [] [ text itemNameEllipsis ]
            in
            tr []
            (
                [ itemNameCell
                , td [] [ text maxSizeHuman ]
                ]
                ++ List.map (agentItemStatusToCell itemName maxSize) agentItemStatusRow
            )
    in
    [ table [ class "ctlisting-table" ]
        (
            [ thead []
                (
                    [ th [] [ text "Item" ]
                    , th [] [ text "Size" ]
                    ]
                    ++ List.map toTableHeader agents
                )
            ]
            ++ List.map (toTableRow agents ctListings stiListings) uniqueNames
        )
    ]


-- UPDATE



type Msg
    = AddSTIAction Agent STIActionClass STIActionType String
    | RemoveSTIAction Agent Int
    | GotAgentCTListing (Result Http.Error (Agent, CTListing.Listing))
    | GotAgentSTIListing (Result Http.Error (Agent, STIListing.Listing))
    | ApplySTIActions
    | FlushSTIActions
    | CloseApplyModal
    | CloseFlushModal
    | GotSession Session


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AddSTIAction agent actionClass actionType name ->
            let
                agentKey = Agents.key agent

                maybeStiActions = Dict.get agentKey model.agentStiActions

                actionSame : STIActionClass -> STIActionType -> String -> STIAction -> Bool
                actionSame newActionClass newActionType newName curAction =
                    if newActionClass /= curAction.actionClass then
                        False
                    else if newActionType /= curAction.actionType then
                        False
                    else if newName /= curAction.name then
                        False
                    else
                        True

                (newStiActions, actionExistsAlready) =
                    case maybeStiActions of
                        (Just stiActions) ->
                            case (List.any (actionSame actionClass actionType name) stiActions) of
                                True ->
                                    (stiActions, True)
                                False ->
                                    ((stiActions ++ [STIAction model.nextStiId actionClass actionType name]), False)
                        Nothing ->
                            ([STIAction model.nextStiId actionClass actionType name], False)

                nextStiId =
                    if actionExistsAlready then
                        model.nextStiId
                    else
                        model.nextStiId + 1

                newAgentStiActions = Dict.insert agentKey newStiActions model.agentStiActions
            in
            ( { model | agentStiActions = newAgentStiActions, nextStiId = nextStiId }
            , Cmd.none
            )

        RemoveSTIAction agent id ->
            let
                isNotId : Int -> STIAction -> Bool
                isNotId needle item =
                    if item.id == needle then
                        False
                    else
                        True

                agentKey = Agents.key agent

                maybeStiActions = Dict.get agentKey model.agentStiActions
            in
            case maybeStiActions of
                Just stiActions ->
                    let
                        newStiActions = List.filter (isNotId id) stiActions
                        newAgentStiActions = Dict.insert agentKey newStiActions model.agentStiActions
                    in
                    ( { model | agentStiActions = newAgentStiActions }, Cmd.none )
                Nothing ->
                    ( model, Cmd.none )

        GotAgentCTListing (Ok (agent, listing)) ->
            let
                kvlisting = CTListing.toKV listing

                agentStr = Agents.pretty agent
                maybeExisting = Dict.get agentStr model.ctListings

                newListings =
                    case maybeExisting of
                        Just current ->
                            Dict.insert agentStr (Dict.union kvlisting current) model.ctListings

                        Nothing ->
                            Dict.insert agentStr kvlisting model.ctListings
            in
            ( { model | ctListings = newListings }, Cmd.none )

        GotAgentCTListing (Err error) ->
            ( model, Cmd.none )

        GotAgentSTIListing (Ok (agent, listing)) ->
            let
                kvlisting = STIListing.toKV listing

                agentStr = Agents.pretty agent
                maybeExisting = Dict.get agentStr model.stiListings

                newListings =
                    case maybeExisting of
                        Just current ->
                            Dict.insert agentStr (Dict.union kvlisting current) model.stiListings

                        Nothing ->
                            Dict.insert agentStr kvlisting model.stiListings
            in
            ( { model | stiListings = newListings }, Cmd.none )

        GotAgentSTIListing (Err error) ->
            ( model, Cmd.none )

        ApplySTIActions ->
            ( { model | applyModalOpen = True }, Cmd.none )

        FlushSTIActions ->
            ( { model | flushModalOpen = True }, Cmd.none )

        CloseApplyModal ->
            ( { model | applyModalOpen = False }, Cmd.none )

        CloseFlushModal ->
            ( { model | flushModalOpen = False }, Cmd.none )

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
