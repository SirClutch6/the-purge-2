module Types.Weapons exposing (..)

import Types.Player as Player
import Types.Enemy as Enemy

type Character
    = Player Player.Player
    | Enemy Enemy.Enemy

type Weapon
    = Taser
    | Pistol
    | MachineGun
    | NightStick
    | Blade
    | Spear
    | Club
    | None
    | Other String

type AttackType
    = Melee
    | Ranged

getTauntDamage : Enemy.Enemy -> Int
getTauntDamage enemy =
    enemy.charisma
    
getWeaponDamage : Character -> Weapon -> AttackType -> Int
getWeaponDamage character weapon range =
    let
        dex = case character of
            Player player ->
                player.dexterity

            Enemy enemy ->
                enemy.dexterity

        str = case character of
            Player player ->
                player.strength

            Enemy enemy ->
                enemy.strength
    in
    case weapon of
        Taser ->
            case range of
                Melee ->
                    dex + 3

                Ranged ->
                    str - 2

        Pistol ->
            case range of
                Melee ->
                    str - 1

                Ranged ->
                    dex + 2

        MachineGun ->
            case range of
                Melee ->
                    str + 0

                Ranged ->
                    dex + 3

        NightStick ->
            case range of
                Melee ->
                    str + 3

                Ranged ->
                    str - 2

        Blade ->
            case range of
                Melee ->
                    dex + 2

                Ranged ->
                    dex + 0

        Spear ->
            case range of
                Melee ->
                    dex + 1

                Ranged ->
                    str + 1

        Club ->
            case range of
                Melee ->
                    str + 2

                Ranged ->
                    str - 2

        None ->
            case range of
                Melee ->
                    let
                        num = toFloat <| max str dex
                    in
                    round (num / 2)

                Ranged ->
                    -1

        Other _ ->
            case range of
                Melee ->
                    str - 1

                Ranged ->
                    str - 2

weaponToString : Weapon -> String
weaponToString weapon =
    case weapon of
        Taser ->
            "Taser"

        Pistol ->
            "Pistol"

        MachineGun ->
            "Machine Gun"

        NightStick ->
            "Night Stick"

        Blade ->
            "Blade"

        Spear ->
            "Spear"

        Club ->
            "Club"

        None ->
            "None"

        Other name ->
            name
