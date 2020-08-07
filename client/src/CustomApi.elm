module CustomApi exposing(..)

import Api exposing (..)

import Json.Decode
import Json.Encode
import Debug exposing (log)
import Dict
import Http
import Url.Builder

headers : String -> List Http.Header
headers token = [ Http.header "Authorization" ("Bearer " ++ token) ]

postApiItem : String -> String -> (Result Http.Error  (ItemId)  -> msg) -> Cmd msg
postApiItem token body toMsg =
    let
        params =
            List.filterMap identity
            (List.concat
                [])
    in
        Http.request
            { method =
                "POST"
            , headers = headers token
            , url =
                Url.Builder.crossOrigin ""
                    [ "api"
                    , "item"
                    ]
                    params
            , body =
                Http.jsonBody (Json.Encode.string body)
            , expect =
                Http.expectJson toMsg jsonDecItemId
            , timeout =
                Nothing
            , tracker =
                Nothing
            }

getApiItem : String -> (Result Http.Error  ((List ItemId))  -> msg) -> Cmd msg
getApiItem token toMsg =
    let
        params =
            List.filterMap identity
            (List.concat
                [])
    in
        Http.request
            { method =
                "GET"
            , headers = headers token
            , url =
                Url.Builder.crossOrigin ""
                    [ "api"
                    , "item"
                    ]
                    params
            , body =
                Http.emptyBody
            , expect =
                Http.expectJson toMsg (Json.Decode.list (jsonDecItemId))
            , timeout =
                Nothing
            , tracker =
                Nothing
            }


getApiItemByItemId : String -> ItemId -> (Result Http.Error  (Item)  -> msg) -> Cmd msg
getApiItemByItemId token capture_itemId toMsg =
    let
        params =
            List.filterMap identity
            (List.concat
                [])
    in
        Http.request
            { method =
                "GET"
            , headers = headers token
            , url =
                Url.Builder.crossOrigin ""
                    [ "api"
                    , "item"
                    , (capture_itemId |> String.fromInt)
                    ]
                    params
            , body =
                Http.emptyBody
            , expect =
                Http.expectJson toMsg jsonDecItem
            , timeout =
                Nothing
            , tracker =
                Nothing
            }

