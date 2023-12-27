module Nonempty.List exposing
    ( NonemptyList
    , any
    , foldr
    , fromList
    , head
    , map
    , push
    , singleton
    , toList
    )


type NonemptyList a
    = NonemptyList ( a, List a )


singleton : a -> NonemptyList a
singleton a =
    NonemptyList ( a, [] )


fromList : List a -> Maybe (NonemptyList a)
fromList list =
    case list of
        [] ->
            Nothing

        a :: rest ->
            Just (NonemptyList ( a, rest ))


toList : NonemptyList a -> List a
toList (NonemptyList ( a, rest )) =
    a :: rest


any : (a -> Bool) -> NonemptyList a -> Bool
any predicate (NonemptyList ( a, rest )) =
    predicate a || List.any predicate rest


map : (a -> b) -> NonemptyList a -> NonemptyList b
map f (NonemptyList ( a, rest )) =
    NonemptyList ( f a, List.map f rest )


foldr : (a -> b -> b) -> b -> NonemptyList a -> b
foldr f b (NonemptyList ( a, rest )) =
    f a (List.foldr f b rest)


push : a -> NonemptyList a -> NonemptyList a
push a (NonemptyList ( a1, rest )) =
    NonemptyList ( a, a1 :: rest )


head : NonemptyList a -> a
head (NonemptyList ( a, _ )) =
    a
