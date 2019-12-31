module DemoUtils exposing
    ( class_
    , onArrowKeysDown
    , updateListItemAtIndex
    )

{-| This module includes some helper/general functions that don't fit in other modules
-}

import Html
import Html.Attributes exposing (classList)
import Html.Events
import Json.Decode


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


{-| Makes it a little simpler to add a CSS class to an element
-}
class_ : String -> Html.Attribute a
class_ className =
    classList [ ( className, True ) ]


{-| Helper for sending specific messages when the user presses an arrow key.
-}
onArrowKeysDown : { onUp : msg, onRight : msg, onDown : msg, onLeft : msg } -> Html.Attribute msg
onArrowKeysDown { onUp, onRight, onDown, onLeft } =
    let
        tagger =
            \keyCode ->
                case keyCode of
                    37 ->
                        Json.Decode.succeed onLeft

                    38 ->
                        Json.Decode.succeed onUp

                    39 ->
                        Json.Decode.succeed onRight

                    40 ->
                        Json.Decode.succeed onDown

                    _ ->
                        -- if any other keystroke, just fail. That way, no message will be produced
                        Json.Decode.fail ""
    in
    Html.Events.on "keydown" (Json.Decode.andThen tagger Html.Events.keyCode)
