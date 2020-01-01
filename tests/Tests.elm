module Tests exposing (..)

import Expect
import ListView
import ListView.Viewers
import Test exposing (..)


type Msg
    = TableMsg ListView.Viewers.ListViewMsg


type alias Row =
    { stringCol : String
    , intCol : Int
    }


tableConfig : ListView.Config Row Msg
tableConfig =
    ListView.makeConfig
        |> ListView.withColumn (ListView.makeColumn.string "a string" .stringCol)
        |> ListView.withColumn (ListView.makeColumn.int "an int" .intCol)


tableState : ListView.State
tableState =
    ListView.makeState


rows : List Row
rows =
    [ { stringCol = "d", intCol = 9 }
    , { stringCol = "m", intCol = 1 }
    , { stringCol = "z", intCol = 7 }
    , { stringCol = "a", intCol = 70 }
    ]



-- Check out https://package.elm-lang.org/packages/elm-explorations/test/latest to learn more about testing in Elm!


getViewInfoForState : ListView.State -> ListView.ListViewInfo Row Msg
getViewInfoForState stateCfg =
    ListView.getViewInfo tableConfig stateCfg rows


getDisplayedRowsForState : ListView.State -> List Row
getDisplayedRowsForState stateCfg =
    getViewInfoForState stateCfg
        |> .rowsToDisplay
        |> List.map Tuple.second


genericTests : Test
genericTests =
    describe "Transforming data for the ListView"
        [ test "does not modify data with default setup" <|
            \_ ->
                tableState
                    |> getDisplayedRowsForState
                    |> Expect.equal rows
        , test "columns are correctly generated" <|
            \_ ->
                tableState
                    |> ListView.updateState.withSorting (ListView.Sorted 1 ListView.DESC)
                    |> getViewInfoForState
                    |> .columnsViewInfo
                    |> Expect.equal
                        [ { name = "a string", code = "a_string", index = 0, sortInfo = ListView.UnsortedColumn }
                        , { code = "an_int", index = 1, name = "an int", sortInfo = ListView.SortedColumn ListView.DESC }
                        ]
        ]


testSorting : Test
testSorting =
    describe "When sorting, "
        [ test "correctly sorts data in ascending direction" <|
            \_ ->
                tableState
                    |> ListView.updateState.withSorting (ListView.Sorted 0 ListView.ASC)
                    |> getDisplayedRowsForState
                    |> List.map .stringCol
                    |> Expect.equal [ "a", "d", "m", "z" ]
        , test "correctly sorts data in descending direction" <|
            \_ ->
                tableState
                    |> ListView.updateState.withSorting (ListView.Sorted 0 ListView.DESC)
                    |> getDisplayedRowsForState
                    |> List.map .stringCol
                    |> Expect.equal [ "z", "m", "d", "a" ]
        ]


testPagination : Test
testPagination =
    describe "When paginating, "
        [ test "correctly shows 3 rows on the first page when page size is 3" <|
            \_ ->
                tableState
                    |> ListView.updateState.withRowsPerPage 3
                    |> ListView.updateState.withPage 4 ListView.GotoFirstPage
                    |> getDisplayedRowsForState
                    |> List.map .stringCol
                    |> Expect.equal [ "d", "m", "z" ]
        , test "correctly shows only one row in the last page when page size is 3" <|
            \_ ->
                tableState
                    |> ListView.updateState.withRowsPerPage 3
                    |> ListView.updateState.withPage 4 ListView.GotoLastPage
                    |> getDisplayedRowsForState
                    |> List.map .stringCol
                    |> Expect.equal [ "a" ]
        , test "correctly shows paging info" <|
            \_ ->
                tableState
                    |> ListView.updateState.withRowsPerPage 3
                    |> ListView.updateState.withPage 4 ListView.GotoLastPage
                    |> getViewInfoForState
                    |> .pagingViewInfo
                    |> Expect.equal
                        { numberOfRows = 4
                        , numberOfPages = 2
                        , rowsPerPage = 3
                        , currentPage = 2
                        , currentPageStartIndex = 3
                        , currentPageEndIndex = 3
                        }
        ]
