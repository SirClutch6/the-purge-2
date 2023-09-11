module Types.VendingItems exposing (..)

import Types.Player as P

type alias Cost
    = Int

type Item
    = Water
    | Juice
    | HotChocolate
    | ProteinShake

type alias Purchaseable =
    { item : Item
    , cost : Cost
    }

possibleItemsToPurchase : List Purchaseable
possibleItemsToPurchase =
    [ Purchaseable Water 2
    , Purchaseable Juice 5
    , Purchaseable HotChocolate 10
    , Purchaseable ProteinShake 20
    ]
    -- = Water 2
    -- | Juice 5
    -- | HotChocolate 10
    -- | ProteinShake 20

purchaseablesToString : Purchaseable -> (String, String)
purchaseablesToString item =
    case item.item of
        Water ->
            ("Water", coinsToString item.cost)

        Juice ->
            ("Juice", coinsToString item.cost)

        HotChocolate ->
            ("Hot Chocolate", coinsToString item.cost)

        ProteinShake ->
            ("Protein Shake", coinsToString item.cost)

coinsToString : Cost -> String
coinsToString cost = 
    case cost of
        2 -> "2 Coins"
        5 -> "5 Coins"
        10 -> "10 Coins"
        20 -> "20 Coins"
        _ -> "Error!"

purchaseableEffect : Purchaseable -> P.Player -> P.Player
purchaseableEffect item player =
    case item.item of
        Water ->
            P.adjustHealth 5 player

        Juice ->
            P.adjustSanity 10 player

        HotChocolate ->
            P.adjustHealth 1000 player |> P.adjustSanity 1000

        ProteinShake ->
            P.adjustMaxHealth 10 player