module Session exposing (Session, changes, cred, fromViewer, navKey, agents, addAgent, removeAgent, viewer)

import Api exposing (Cred)
import Avatar exposing (Avatar)
import Browser.Navigation as Nav
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (custom, required)
import Json.Encode as Encode exposing (Value)
import Profile exposing (Profile)
import Time
import Viewer exposing (Viewer)
import Agents exposing (Agents, Agent)



-- TYPES


type Session
    = LoggedIn Nav.Key Viewer Agents
    | Guest Nav.Key Agents



-- INFO


viewer : Session -> Maybe Viewer
viewer session =
    case session of
        LoggedIn _ val _ ->
            Just val

        Guest _ _ ->
            Nothing


cred : Session -> Maybe Cred
cred session =
    case session of
        LoggedIn _ val _ ->
            Just (Viewer.cred val)

        Guest _ _ ->
            Nothing


navKey : Session -> Nav.Key
navKey session =
    case session of
        LoggedIn key _ _ ->
            key

        Guest key _ ->
            key


agents : Session -> Agents
agents session =
    case session of
        LoggedIn _ _ val ->
            val

        Guest _ val ->
            val



-- CHANGES


addAgent : Session -> Agent -> Session
addAgent session agent =
    case session of
        LoggedIn key maybeCred curAgents ->
            LoggedIn key maybeCred (List.append curAgents [agent])

        Guest key curAgents ->
            Guest key (List.append curAgents [agent])


sameAgent : Agent -> Agent -> Bool
sameAgent (Agents.Agent aInt) (Agents.Agent bInt) =
    if aInt.name /= bInt.name then
        False
    else if aInt.host /= bInt.host then
        False
    else
        True


notSameAgent : Agent -> Agent -> Bool
notSameAgent aAgent bAgent =
    not (sameAgent aAgent bAgent)


removeAgent : Session -> Agent -> Session
removeAgent session agent =
    case session of
        LoggedIn key maybeCred curAgents ->
            LoggedIn key maybeCred (List.filter (notSameAgent agent) curAgents)

        Guest key curAgents ->
            Guest key (List.filter (notSameAgent agent) curAgents)


changes : (Session -> msg) -> Nav.Key -> Sub msg
changes toMsg key =
    Api.viewerChanges (\maybeViewer -> toMsg (fromViewer key maybeViewer)) Viewer.decoder


fromViewer : Nav.Key -> Maybe Viewer -> Session
fromViewer key maybeViewer =
    case maybeViewer of
        Just viewerVal ->
            LoggedIn key viewerVal []

        Nothing ->
            Guest key []
