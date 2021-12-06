module STIAction exposing (STIActionClass(..), STIActionType(..), STIAction)

import Agents exposing (Agent)

type STIActionClass
    = Add
    | Remove


type STIActionType
    = Ignore
    | Keep


type alias STIAction =
    { id : Int
    , agent : Agent
    , actionClass : STIActionClass
    , actionType : STIActionType
    , name : String
    }
