module Rga exposing
    ( Rga, init, fromList, toList
    , insert, update, delete
    , RemoteOp, apply
    , test1, test2
    )

{-|

@docs Rga, init, fromList, toList
@docs insert, update, delete
@docs RemoteOp, apply

-}

import Dict exposing (Dict)
import Set exposing (Set)


type alias Rga a =
    { session : Int
    , site : Int
    , clock : VectorClock
    , nodes : Dict String (Node a)
    , head : Maybe String
    }


type alias Node a =
    { value : Maybe a
    , id : SVector
    , precedence : SVector
    , next : Maybe String
    }


type alias VectorClock =
    Dict Int Int


type alias SVector =
    { session : Int
    , site : Int
    , sum : Int
    , sequence : Int
    }


init : Int -> Set Int -> Rga a
init site sites =
    { session = 1
    , site = site
    , clock = sites |> Set.insert site |> Set.foldl (\n -> Dict.insert n 0) Dict.empty
    , nodes = Dict.empty
    , head = Nothing
    }


toList : Rga a -> List a
toList { nodes, head } =
    foldlHelp nodes head (::) [] |> List.reverse


foldl : (a -> b -> b) -> b -> Rga a -> b
foldl f result { nodes, head } =
    foldlHelp nodes head f result


foldlHelp : Dict String (Node a) -> Maybe String -> (a -> b -> b) -> b -> b
foldlHelp nodes next f result =
    case next |> Maybe.andThen (\key -> Dict.get key nodes) of
        Just node ->
            foldlHelp
                nodes
                node.next
                f
                (case node.value of
                    Just x ->
                        f x result

                    Nothing ->
                        result
                )

        Nothing ->
            result


fromList : Int -> Set Int -> List a -> Rga a
fromList site sites list =
    { session = 1
    , site = site
    , clock = sites |> Set.insert site |> Set.foldl (\n -> Dict.insert n 0) Dict.empty
    , nodes = nodesFromList list 0 Dict.empty
    , head =
        if List.isEmpty list then
            Nothing

        else
            Just (svectorKey (initSVector 0))
    }


nodesFromList : List a -> Int -> Dict String (Node a) -> Dict String (Node a)
nodesFromList list sequence nodes =
    case list of
        value :: rest ->
            let
                vec =
                    initSVector sequence

                next =
                    case rest of
                        [] ->
                            Nothing

                        _ ->
                            Just (svectorKey (initSVector (sequence + 1)))

                node =
                    Node (Just value) vec vec next
            in
            nodes |> Dict.insert (svectorKey vec) node |> nodesFromList rest (sequence + 1)

        [] ->
            nodes


initSVector : Int -> SVector
initSVector sequence =
    SVector 0 0 sequence sequence


svectorKey : SVector -> String
svectorKey { session, site, sum, sequence } =
    [ session, site, sum, sequence ] |> List.map String.fromInt |> String.join "_"


svectorPrecedes : SVector -> SVector -> Bool
svectorPrecedes a b =
    (a.session < b.session)
        || (a.session == b.session && a.sum < b.sum)
        || (a.session == b.session && a.sum == b.sum && a.site < b.site)


causallyReady : VectorClock -> Int -> VectorClock -> Bool
causallyReady clock siteOp clockOp =
    Dict.merge
        (\_ _ _ -> False)
        (\site t tOp ready ->
            ready
                && (if site == siteOp then
                        tOp == t + 1

                    else
                        tOp <= t
                   )
        )
        (\_ _ _ -> False)
        clock
        clockOp
        True


nextSVector : Rga a -> SVector
nextSVector { session, site, clock } =
    SVector
        session
        site
        (clock |> Dict.foldl (always (+)) 1)
        (clock |> Dict.get site |> Maybe.withDefault 0 |> (+) 1)


lookup : Dict comparable v -> comparable -> Maybe v
lookup dict key =
    Dict.get key dict


find : Int -> Rga a -> Maybe (Node a)
find i rga =
    if i <= 0 then
        Nothing

    else
        rga.head |> Maybe.andThen (findHelp rga.nodes (i - 1))


findHelp : Dict String (Node a) -> Int -> String -> Maybe (Node a)
findHelp nodes i key =
    case nodes |> Dict.get key of
        Just node ->
            if i == 0 && node.value /= Nothing then
                Just node

            else
                case node.next of
                    Just nextKey ->
                        findHelp
                            nodes
                            (if node.value == Nothing then
                                i

                             else
                                i - 1
                            )
                            nextKey

                    Nothing ->
                        Nothing

        Nothing ->
            Nothing



-- insert


insert : Int -> a -> Rga a -> Maybe ( Rga a, RemoteOp a )
insert i value rga =
    let
        left =
            find i rga
    in
    if i == 0 || left /= Nothing then
        Just
            (( rga |> insertHelp (nextSVector rga) left value
             , Insert (left |> Maybe.map .id) value
             )
                |> toRemoteOp
            )

    else
        Nothing


insertHelp : SVector -> Maybe (Node a) -> a -> Rga a -> Rga a
insertHelp vec left value rga =
    let
        key =
            svectorKey vec

        clock =
            rga.clock |> Dict.insert vec.site vec.sequence
    in
    case left of
        Nothing ->
            -- insert at head
            { rga
                | clock = clock
                , nodes =
                    rga.nodes
                        |> Dict.insert key (Node (Just value) vec vec rga.head)
                , head = Just key
            }

        Just node ->
            -- insert after node
            { rga
                | clock = clock
                , nodes =
                    rga.nodes
                        |> Dict.insert (svectorKey node.id) { node | next = Just key }
                        |> Dict.insert key (Node (Just value) vec vec node.next)
            }


remoteInsert : SVector -> Maybe SVector -> a -> Rga a -> Rga a
remoteInsert vec left value rga =
    let
        leftNode =
            left |> Maybe.andThen (svectorKey >> lookup rga.nodes)

        maybeRightNode =
            leftNode
                |> Maybe.map .next
                |> Maybe.withDefault rga.head
                |> Maybe.andThen (lookup rga.nodes)

        newLeftNode =
            maybeRightNode
                |> Maybe.map (skipSucceeding rga.nodes vec leftNode)
                |> Maybe.withDefault leftNode
    in
    rga |> insertHelp vec newLeftNode value


skipSucceeding : Dict String (Node a) -> SVector -> Maybe (Node a) -> Node a -> Maybe (Node a)
skipSucceeding nodes vec left right =
    -- once we find a right < vec then we have the left we want, otherwise keep advancing
    if svectorPrecedes right.id vec then
        left

    else
        case right.next |> Maybe.andThen (lookup nodes) of
            Just afterRight ->
                skipSucceeding nodes vec (Just right) afterRight

            Nothing ->
                Just right



-- update


update : Int -> a -> Rga a -> Maybe ( Rga a, RemoteOp a )
update i value rga =
    find i rga
        |> Maybe.map
            (\node ->
                ( rga |> updateHelp (nextSVector rga) node value
                , Update node.id value
                )
                    |> toRemoteOp
            )


updateHelp : SVector -> Node a -> a -> Rga a -> Rga a
updateHelp vec node value rga =
    { rga
        | clock = rga.clock |> Dict.insert vec.site vec.sequence
        , nodes =
            rga.nodes
                |> Dict.insert (svectorKey node.id) { node | value = Just value, precedence = vec }
    }


remoteUpdate : SVector -> SVector -> a -> Rga a -> Rga a
remoteUpdate vec target value rga =
    case Dict.get (svectorKey target) rga.nodes of
        Just node ->
            if svectorPrecedes node.precedence vec then
                rga |> updateHelp vec node value

            else
                rga

        Nothing ->
            rga



-- delete


delete : Int -> Rga a -> Maybe ( Rga a, RemoteOp a )
delete i rga =
    find i rga
        |> Maybe.map
            (\node ->
                ( rga |> deleteHelp (nextSVector rga) node
                , Delete node.id
                )
                    |> toRemoteOp
            )


deleteHelp : SVector -> Node a -> Rga a -> Rga a
deleteHelp vec node rga =
    { rga
        | clock = rga.clock |> Dict.insert vec.site vec.sequence
        , nodes =
            rga.nodes
                |> Dict.insert (svectorKey node.id) { node | value = Nothing, precedence = vec }
    }


remoteDelete : SVector -> SVector -> Rga a -> Rga a
remoteDelete vec target rga =
    case Dict.get (svectorKey target) rga.nodes of
        Just node ->
            rga |> deleteHelp vec node

        Nothing ->
            rga



--


toRemoteOp : ( Rga a, Op a ) -> ( Rga a, RemoteOp a )
toRemoteOp ( rga, op ) =
    ( rga
    , RemoteOp
        rga.session
        rga.site
        rga.clock
        op
    )


type alias RemoteOp a =
    { session : Int
    , site : Int
    , clock : VectorClock
    , op : Op a
    }


type Op a
    = Insert (Maybe SVector) a
    | Update SVector a
    | Delete SVector


apply : RemoteOp a -> Rga a -> Rga a
apply remote rga =
    let
        vec =
            SVector
                remote.session
                remote.site
                (remote.clock |> Dict.foldl (always (+)) 0)
                (remote.clock |> Dict.get remote.site |> Maybe.withDefault 0)
    in
    case remote.op of
        Insert left value ->
            rga |> remoteInsert vec left value

        Update target value ->
            rga |> remoteUpdate vec target value

        Delete target ->
            rga |> remoteDelete vec target



-------------------------------------------------------------------------------


test1 =
    let
        sites =
            Set.fromList [ 0, 1, 2 ]

        list =
            [ 'a', 'b' ]

        o0 =
            RemoteOp 0 0 Dict.empty (Delete (initSVector 0))

        --
        a =
            fromList 0 sites list

        b =
            fromList 1 sites list

        c =
            fromList 2 sites list

        --
        ( b1, o2 ) =
            b |> insert 1 '2' |> Maybe.withDefault ( b, o0 )

        ( c1, o3 ) =
            c |> insert 1 '3' |> Maybe.withDefault ( c, o0 )

        a1 =
            a |> apply o3

        --
        b2 =
            b1 |> apply o3

        c2 =
            c1 |> apply o2

        ( a2, o1 ) =
            a1 |> insert 1 '1' |> Maybe.withDefault ( a1, o0 )

        --
        b3 =
            b2 |> apply o1

        c3 =
            c2 |> apply o1

        a3 =
            a2 |> apply o2

        --
        result =
            { a = [ a, a1, a2, a3 ] |> List.map toList
            , b = [ a, b1, b2, b3 ] |> List.map toList
            , c = [ a, c1, c2, c3 ] |> List.map toList
            }

        expected =
            { a = [ [ 'a', 'b' ], [ 'a', '3', 'b' ], [ 'a', '1', '3', 'b' ], [ 'a', '1', '3', '2', 'b' ] ]
            , b = [ [ 'a', 'b' ], [ 'a', '2', 'b' ], [ 'a', '3', '2', 'b' ], [ 'a', '1', '3', '2', 'b' ] ]
            , c = [ [ 'a', 'b' ], [ 'a', '3', 'b' ], [ 'a', '3', '2', 'b' ], [ 'a', '1', '3', '2', 'b' ] ]
            }
    in
    ( result == expected, result )


test2 =
    let
        sites =
            Set.fromList [ 0, 1, 2 ]

        list =
            [ 'a' ]

        o0 =
            RemoteOp 0 0 Dict.empty (Delete (initSVector 0))

        --
        a =
            fromList 0 sites list

        b =
            fromList 1 sites list

        c =
            fromList 2 sites list

        --
        ( a1, u1 ) =
            a |> update 1 '1' |> Maybe.withDefault ( a, o0 )

        ( b1, u2 ) =
            b |> update 1 '2' |> Maybe.withDefault ( b, o0 )

        ( c1, d3 ) =
            c |> delete 1 |> Maybe.withDefault ( c, o0 )

        --
        a2 =
            a1 |> apply u2

        ( b2, i5 ) =
            b1 |> insert 1 '5' |> Maybe.withDefault ( b1, o0 )

        c2 =
            c1 |> apply u1

        --
        a3 =
            a2 |> apply d3

        b3 =
            b2 |> apply u1

        ( a4, i4 ) =
            a3 |> insert 0 '4' |> Maybe.withDefault ( a3, o0 )

        c3 =
            c2 |> apply i4

        --
        b4 =
            b3 |> apply d3

        c4 =
            c3 |> apply u2

        --
        a5 =
            a4 |> apply i5

        b5 =
            b4 |> apply i4

        c5 =
            c4 |> apply i5
    in
    { a = [ a, a1, a2, a3, a4, a5 ] |> List.map toList
    , b = [ b, b1, b2, b3, b4, b5 ] |> List.map toList
    , c = [ c, c1, c2, c3, c4, c5 ] |> List.map toList
    }
