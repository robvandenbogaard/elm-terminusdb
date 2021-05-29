module TerminusDb exposing
    ( databasePath, graphPath, graphType
    , reference, withRef
    , Database(..), Graph(..), GraphType(..)
    , RepoReference(..), Reference(..)
    )

{-| This module provides types and helpers for building url paths to data on a
TerminusDB server.

@docs databasePath, graphPath, graphType

@docs reference, withRef

@docs Database, Graph, GraphType

@docs RepoReference, Reference

-}


{-| The Database type includes the system databases and user created ones. User
databases are prefixed by an account name, often the name of the associated
organisation.
-}
type Database
    = Terminus
    | System
    | Database String String


{-| Constructs a list of url path segments for selecting the specified database.
-}
databasePath : Database -> List String
databasePath d =
    case d of
        Terminus ->
            []

        System ->
            [ "_system" ]

        Database org db ->
            [ org, db ]


{-| Within one database multiple data Graphs can coexist, defaulting to a single
MainGraph with subgraphs for instances, the schema and inferences. Any
additional graphs are described by CustomGraph values, taking a name String and
a subgraph type (Instance, Schema or Inference).
-}
type Graph
    = MainGraph GraphType
    | CustomGraph String GraphType


{-| The Instance GraphType holds the actual data of a database, where a Schema
graph describes the constraints the data should adhere to (the types). I guess
I need to read more on or ask around about the Inference graphs, because I
realise I don't exactly know its role.
-}
type GraphType
    = Instance
    | Schema
    | Inference


graphPath g =
    case g of
        MainGraph t ->
            [ "main", graphType t ]

        CustomGraph c t ->
            [ c, graphType t ]


graphType t =
    case t of
        Instance ->
            "instance"

        Schema ->
            "schema"

        Inference ->
            "inference"


type RepoReference
    = Local Reference
    | Remote String Reference


type Reference
    = Main
    | Branch String
    | Commit String


reference ref =
    case ref of
        Local Main ->
            [ "local", "branch", "main" ]

        Local (Branch b) ->
            [ "local", "branch", b ]

        Remote remote Main ->
            [ remote, "branch", "main" ]

        Remote remote (Branch b) ->
            [ remote, "branch", b ]

        Local (Commit c) ->
            [ "local", "commit", c ]

        Remote remote (Commit c) ->
            [ remote, "commit", c ]


withRef ref segments =
    segments ++ reference ref
