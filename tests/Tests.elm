module Tests exposing (test1, test2)

import Dict exposing (Dict)
import Rga exposing (RemoteOp, Rga)
import Set exposing (Set)


test1 =
    let
        initial =
            [ 'a', 'b' ]

        actions =
            [ ( 0, [ Receive "I3", Perform "I1" (I 1 '1'), Receive "I2" ] )
            , ( 1, [ Perform "I2" (I 1 '2'), Receive "I3", Receive "I1" ] )
            , ( 2, [ Perform "I3" (I 1 '3'), Receive "I2", Receive "I1" ] )
            ]
                |> Dict.fromList

        result =
            actions |> runActions initial

        expected =
            [ ( 0, [ [ 'a', 'b' ], [ 'a', '3', 'b' ], [ 'a', '1', '3', 'b' ], [ 'a', '1', '3', '2', 'b' ] ] )
            , ( 1, [ [ 'a', 'b' ], [ 'a', '2', 'b' ], [ 'a', '3', '2', 'b' ], [ 'a', '1', '3', '2', 'b' ] ] )
            , ( 2, [ [ 'a', 'b' ], [ 'a', '3', 'b' ], [ 'a', '3', '2', 'b' ], [ 'a', '1', '3', '2', 'b' ] ] )
            ]
                |> Dict.fromList
    in
    ( result == expected, result )


test2 =
    let
        initial =
            [ 'a' ]

        actions =
            [ ( 0, [ Perform "U1" (U 1 '1'), Receive "U2", Receive "D3", Perform "I4" (I 0 '4'), Receive "I5" ] )
            , ( 1, [ Perform "U2" (U 1 '2'), Perform "I5" (I 1 '5'), Receive "U1", Receive "D3", Receive "I4" ] )
            , ( 2, [ Perform "D3" (D 1), Receive "U1", Receive "I4", Receive "U2", Receive "I5" ] )
            ]
                |> Dict.fromList

        result =
            actions |> runActions initial

        expected =
            [ ( 0, [ [ 'a' ], [ '1' ], [ '2' ], [], [ '4' ], [ '4', '5' ] ] )
            , ( 1, [ [ 'a' ], [ '2' ], [ '2', '5' ], [ '2', '5' ], [ '5' ], [ '4', '5' ] ] )
            , ( 2, [ [ 'a' ], [], [], [], [ '4' ], [ '4', '5' ] ] )
            ]
                |> Dict.fromList
    in
    ( result == expected, result )



--


type Op a
    = I Int a
    | U Int a
    | D Int


applyOp : Op a -> Rga a -> Maybe ( Rga a, RemoteOp a )
applyOp op rga =
    case op of
        I i value ->
            rga |> Rga.insert i value

        U i value ->
            rga |> Rga.update i value

        D i ->
            rga |> Rga.delete i


type Action a
    = Perform String (Op a)
    | Receive String


runActions : List a -> Dict Int (List (Action a)) -> Dict Int (List (List a))
runActions initial siteActions =
    let
        sites =
            siteActions |> Dict.keys |> Set.fromList
    in
    siteActions
        |> Dict.map (\site actions -> ( [], Rga.fromList site sites initial, actions ))
        |> iterateActions Dict.empty


iterateActions : Dict String (RemoteOp a) -> Dict Int ( List (Rga a), Rga a, List (Action a) ) -> Dict Int (List (List a))
iterateActions ops1 results1 =
    let
        ( done, ops, results ) =
            -- for each site
            Dict.foldl
                (\site ( history, rga, actions ) ( d, o, r ) ->
                    -- attempt the next action
                    case actions of
                        [] ->
                            ( d, o, r )

                        action :: rest ->
                            case action of
                                Perform key op ->
                                    case rga |> applyOp op of
                                        Nothing ->
                                            ( d, o, r )

                                        Just ( nextRga, remoteOp ) ->
                                            ( False, o |> Dict.insert key remoteOp, r |> Dict.insert site ( rga :: history, nextRga, rest ) )

                                Receive key ->
                                    case o |> Dict.get key of
                                        Nothing ->
                                            ( d, o, r )

                                        Just remoteOp ->
                                            ( False, o, r |> Dict.insert site ( rga :: history, rga |> Rga.apply remoteOp, rest ) )
                )
                ( True, ops1, results1 )
                results1
    in
    if done then
        results |> Dict.map (\_ ( history, rga, _ ) -> rga :: history |> List.reverse |> List.map Rga.toList)

    else
        iterateActions ops results
