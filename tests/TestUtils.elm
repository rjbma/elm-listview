module TestUtils exposing (..)

import Expect
import Fuzz
import Test exposing (..)
import Utils exposing (subList)


testListUtils : Test
testListUtils =
    describe "subList"
        [ test "works on empty lists" <|
            \_ ->
                subList 0 2 []
                    |> Expect.equal []
        , Test.fuzz2 Fuzz.int Fuzz.int "works on empty list" <|
            \i1 i2 ->
                subList i1 i2 []
                    |> Expect.equal []

        , test "works on regular list" <|
            \_ ->
                subList 1 3 [ 0, 1, 2, 3, 4, 5, 6, 7, 8, 9 ]
                    |> Expect.equal [ 1, 2 ]
        , test "works on regular list with negative index" <|
            \_ ->
                subList -100 2 [ 0, 1, 2, 3, 4, 5, 6, 7, 8, 9 ]
                    |> Expect.equal [ 0, 1 ]
        , test "works on regular list with index that's too big" <|
            \_ ->
                subList 5 200 [ 0, 1, 2, 3, 4, 5, 6, 7, 8, 9 ]
                    |> Expect.equal [ 5, 6, 7, 8, 9 ]
        ]
