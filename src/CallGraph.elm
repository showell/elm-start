module CallGraph exposing (..)

import Dict exposing (Dict)
import Set exposing (Set)


type alias Ast =
    { outerArgs : Set String
    , lets :
        List
            { name : String
            , deps : Set String
            }
    }


type alias Graph =
    Dict String (Set String)


testAst : Ast
testAst =
    { outerArgs =
        [ "a"
        , "b"
        , "c"
        ]
            |> Set.fromList
    , lets =
        [ { name = "double"
          , deps =
                [ "x"
                ]
                    |> Set.fromList
          }
        , { name = "timesThree"
          , deps =
                [ "x"
                , "three"
                ]
                    |> Set.fromList
          }
        , { name = "compute"
          , deps =
                [ "double"
                , "a"
                , "timesThree"
                , "b"
                , "fac"
                , "c"
                ]
                    |> Set.fromList
          }
        , { name = "wrapCompute"
          , deps =
                [ "compute"
                , "wrapCompute" -- recursion, yuk!
                ]
                    |> Set.fromList
          }
        ]
    }


getAllDescendants : Graph -> Set String -> Set String
getAllDescendants graph startNodes =
    let
        f info =
            case info.workQueue |> Set.toList of
                [] ->
                    info

                h :: rest ->
                    let
                        newNodes =
                            Dict.get h graph
                                |> Maybe.withDefault Set.empty

                        visited =
                            Set.insert
                                h
                                info.visited

                        workQueue =
                            rest
                                |> Set.fromList
                                |> Set.union newNodes
                                |> (\wq -> Set.diff wq visited)

                        nodes =
                            Set.union
                                info.nodes
                                newNodes
                    in
                    f
                        { workQueue = workQueue
                        , nodes = nodes
                        , visited = visited
                        }
    in
    f
        { workQueue = startNodes
        , nodes = startNodes
        , visited = Set.empty
        }
        |> .nodes


reverseGraph : Graph -> Graph
reverseGraph origDict =
    let
        newDict =
            Dict.empty

        expand ( k, vals ) dct =
            Set.foldl
                (\v d ->
                    case Dict.get v d of
                        Just curr ->
                            Dict.insert
                                v
                                (Set.insert k curr)
                                d

                        Nothing ->
                            Dict.insert
                                v
                                (Set.singleton k)
                                d
                )
                dct
                vals
    in
    List.foldl expand newDict (origDict |> Dict.toList)


getTrueLocals : Ast -> Set String
getTrueLocals ast =
    let
        localFuncNames =
            ast.lets
                |> List.map .name
                |> Set.fromList

        -- does our let function directly depend on any
        -- of the outerArgs?
        isDirectDep letF =
            Set.intersect letF.deps ast.outerArgs
                |> Set.isEmpty
                |> not

        -- exclude toplevel deps; they're just noise
        localDeps letF =
            Set.intersect
                letF.deps
                localFuncNames

        directLocals =
            ast.lets
                |> List.filter isDirectDep
                |> List.map .name
                |> Set.fromList

        depGraph =
            ast.lets
                |> List.map
                    (\letF ->
                        ( letF.name
                        , localDeps letF
                        )
                    )
                |> Dict.fromList

        reversedGraph =
            depGraph
                |> reverseGraph

        allTrueLocals =
            getAllDescendants reversedGraph directLocals
    in
    allTrueLocals
