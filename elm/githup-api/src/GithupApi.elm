module GithupApi exposing (..)

import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)


type alias User =
    { id : Int
    , name : Maybe String
    , login : String
    , avatarUrl : String
    , siteAdmin : Bool
    }


type Side
    = Left
    | Right


type alias Comment =
    { id : Int
    , pullRequestReviewId : Maybe Int
    , user : User
    , body : String
    , side : Side
    , links : Dict String String
    }


decodeId : Decoder Int
decodeId =
    Decode.field "id" Decode.int


decodeName : Decoder (Maybe String)
decodeName =
    -- Decode.oneOf
    --     [ Decode.field "name" Decode.string |> Decode.map Just
    --     , Decode.succeed Nothing
    --     ]
    Decode.maybe (Decode.field "name" Decode.string)

decodeUser : Decoder User
decodeUser =
    Decode.map5 User
        decodeId
        decodeName
        (Decode.field "login" Decode.string)
        (Decode.field "avatar_url" Decode.string)
        (Decode.field "site_admin" Decode.bool)


decodePullRequestReviewId : Decoder (Maybe Int)
decodePullRequestReviewId =
    Decode.field "pull_request_review_id" (Decode.nullable Decode.int)


decodeSide : Decoder Side
decodeSide =
    Decode.field "side" Decode.string
        |> Decode.andThen
            (\side ->
                case side of
                    "LEFT" -> Decode.succeed Left
                    "RIGHT" -> Decode.succeed Right
                    _ -> Decode.fail "Invalid side"
            )


decodeLinks : Decoder (Dict String String)
decodeLinks =
    let decodeHref = Decode.field "href" Decode.string
    in
    Decode.field "_links" (Decode.dict decodeHref)


decodeComment : Decoder Comment
decodeComment =
    Decode.map6 Comment
        decodeId
        decodePullRequestReviewId
        (Decode.field "user" decodeUser)
        (Decode.field "body" Decode.string)
        decodeSide
        decodeLinks


decodeComments : Decoder (List Comment)
decodeComments =
    Decode.list decodeComment


encodeUser : User -> Value
encodeUser user =
    let
        name =
            case user.name of
                Nothing -> []
                Just n -> [ ( "name", Encode.string n ) ]
    in
    Encode.object
        (name
            ++ [ ( "id", Encode.int user.id )
               , ( "login", Encode.string user.login )
               , ( "avatar_url", Encode.string user.avatarUrl )
               , ( "site_admin", Encode.bool user.siteAdmin )
               ]
        )


encodeSide : Side -> Value
encodeSide side =
    Encode.string 
        <| case side of
            Left -> "LEFT"
            Right -> "RIGHT"


encodeLinks : Dict String String -> Value
encodeLinks links =
    let toHref = (\_ value -> Encode.object [ ( "href", Encode.string value ) ]) 
    in
    Dict.map toHref links
        |> Dict.toList
        |> Encode.object


encodeComment : Comment -> Value
encodeComment comment =
    let
        prId =
            case comment.pullRequestReviewId of
                Nothing -> Encode.null
                Just id -> Encode.int id
    in
    Encode.object
        [ ( "id", Encode.int comment.id )
        , ( "pull_request_review_id", prId )
        , ( "user", encodeUser comment.user )
        , ( "body", Encode.string comment.body )
        , ( "side", encodeSide comment.side )
        , ( "_links", encodeLinks comment.links )
        ]
