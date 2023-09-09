module Types.VendingItems exposing (..)

import Player as P

type Cost
    = Int

type Purchaseable Cost
    = Water 2
    | Juice 5
    | HotChocolate 10
    | ProteinShake 20

purchaseablesToString : Purchaseable Cost -> (String, String)
purchaseablesToString item =
    case item of
        Water cost ->
            ("Water", toString cost ++ " Coins")

        Juice cost ->
            ("Juice", toString cost ++ " Coins")

        HotChocolate cost ->
            ("Hot Chocolate", toString cost ++ " Coins")

        ProteinShake cost ->
            ("Protein Shake", toString cost ++ " Coins")

purchaseableEffect : Purchaseable Cost -> P.Player -> P.Player
purchaseableEffect item player =
    case item of
        Water cost ->
            P.adjustHealth 5 player

        Juice cost ->
            P.adjustSanity 10 player

        HotChocolate cost ->
            P.addHealth 1000 player |> P.adjustSanity 1000

        ProteinShake cost ->
            P.adjustMaxHealth 10 player