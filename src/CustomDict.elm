module CustomDict exposing (CustomDict, empty, insert, update, get)

import Dict


type alias CustomDict k v =
    { hash : k -> Int
    , dict : Dict.Dict Int v
    }


empty : (k -> Int) -> CustomDict k v
empty hash =
    { hash = hash, dict = Dict.empty }


insert : k -> v -> CustomDict k v -> CustomDict k v
insert key value dictionary =
    let
        lookup =
            dictionary.hash key

        dict =
            Dict.insert lookup value dictionary.dict
    in
        { dictionary | dict = dict }


update : k -> (Maybe v -> Maybe v) -> CustomDict k v -> CustomDict k v
update key update dictionary =
    let
        lookup =
            dictionary.hash key

        dict =
            Dict.update lookup update dictionary.dict
    in
        { dictionary | dict = dict }


get : k -> (CustomDict k v) -> Maybe v
get key dictionary =
    let
        lookup =
            dictionary.hash key

        result =
            Dict.get lookup dictionary.dict
    in
       result


