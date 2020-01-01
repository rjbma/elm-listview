module Utils exposing
    ( Comparer
    , class_
    , codify
    , comparing
    , findFirst
    , getIndexFromList
    , subList
    , toggleSorter
    , tupleApply2
    , updateListItemAtIndex
    , zip
    )

import Html
import Html.Attributes exposing (classList)


{-| This module includes some helper/general functions that don't fit in other modules
-}



-- Comparison utilities
-- This comes straight from <https://stackoverflow.com/a/42965832/2549949>.
-- I don't have enough reputation on StackOverflow to upvote, so if you happen to see this, thanks Chad Gilbert, you saved my life!


type alias Comparer a =
    a -> a -> Order


comparing : (a -> comparable) -> Comparer a
comparing toComparable a1 a2 =
    compare (toComparable a1) (toComparable a2)


{-| Returns the inverse of a comparer function. Useful to toggle the sort order
-}
toggleSorter : Comparer a -> a -> a -> Order
toggleSorter comp a1 a2 =
    case comp a1 a2 of
        LT ->
            GT

        GT ->
            LT

        EQ ->
            EQ


{-| Gets the element with the given index from a list, if any
-}
getIndexFromList : List a -> Int -> Maybe a
getIndexFromList list index =
    getIndexFromListAux list index 0


getIndexFromListAux : List a -> Int -> Int -> Maybe a
getIndexFromListAux list index currentIndex =
    case list of
        fst :: rest ->
            if currentIndex == index then
                Just fst

            else
                getIndexFromListAux rest index (currentIndex + 1)

        [] ->
            Nothing


{-| Returns a new list where the item with a specific index is updated
according the given updater function.
-}
updateListItemAtIndex : (a -> a) -> Int -> List a -> List a
updateListItemAtIndex updater itemIndex values =
    List.indexedMap
        (\index item ->
            if index == itemIndex then
                updater item

            else
                item
        )
        values


{-| Get the first element of a list for which the predicate returns `True`
-}
findFirst : (a -> Bool) -> List a -> Maybe a
findFirst predicate list =
    case list of
        fst :: rest ->
            if predicate fst then
                Just fst

            else
                findFirst predicate rest

        [] ->
            Nothing


{-| Get a specific slice of a list. Useful for pagination.
-}
subList : Int -> Int -> List a -> List a
subList begin end list =
    subListAux begin end 0 list


subListAux : Int -> Int -> Int -> List a -> List a
subListAux begin end current list =
    case list of
        [] ->
            []

        fst :: rst ->
            if begin > end then
                []

            else if current > end then
                []

            else if current >= begin && current < end then
                fst :: subListAux begin end (current + 1) rst

            else
                subListAux begin end (current + 1) rst


{-| Makes it a little simpler to add a CSS class to an element
-}
class_ : String -> Html.Attribute a
class_ className =
    classList [ ( className, True ) ]


{-| Transforms the given `String` into another one that can be used in a CSS class.

This function is used to try and come up with a more _normalized_ name for a column; one
that can be used in a CSS class, for example.

The algorithm, however, is really simple and doesn't cover many cases. Basically, it just
replaces all spaces by underscores and lowers every char. For example, it doesn't strip
special chars.

If you use column names that include special chars, or if you simply want your columns to
have better CSS classes, you can specify the code with `ListView.updateColumn.withCode`.

Note that neither this function nor the whole `ListView` library makes any guarantee that
each column has a unique code!

    codify "Some Name" == "some_name"

-}
codify : String -> String
codify =
    String.replace " " "_"
        >> String.toLower


{-| Helper function for zipping two lists into one.
It uses `List.map2`, so if one list is longer, the extra elements are dropped
-}
zip : List x -> List y -> List ( x, y )
zip xs ys =
    List.map2 Tuple.pair xs ys


{-| Apply the elements of a binary tuple to a function
-}
tupleApply2 : (a -> b -> result) -> ( a, b ) -> result
tupleApply2 fn ( a, b ) =
    fn a b
