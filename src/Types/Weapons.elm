module Types.Weapons exposing (..)

import Player as Player
import Enemy as Enemy

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

getWeaponDamage : Character -> Weapon -> AttackType -> Int
getWeaponDamage character weapon range =
    case weapon of
        Taser ->
            case range of
                Melee ->
                    character.dexterity + 3

                Ranged ->
                    character.strength - 2

        Pistol ->
            case range of
                Melee ->
                    character.strength - 1

                Ranged ->
                    character.dexterity + 2

        MachineGun ->
            case range of
                Melee ->
                    character.strength + 0

                Ranged ->
                    character.dexterity + 3

        NightStick ->
            case range of
                Melee ->
                    character.strength + 3

                Ranged ->
                    character.strength - 2

        Blade ->
            case range of
                Melee ->
                    character.dexterity + 2

                Ranged ->
                    character.dexterity + 0

        Spear ->
            case range of
                Melee ->
                    character.dexterity + 1

                Ranged ->
                    character.strength + 1

        Club ->
            case range of
                Melee ->
                    character.strength + 2

                Ranged ->
                    character.strength - 2

        None ->
            case range of
                Melee ->
                    let
                        num = max character.strength character.dexterity
                    in
                    round (num / 2)

                Ranged ->
                    -1

        Other _ ->
            case range of
                Melee ->
                    character.strength - 1

                Ranged ->
                    character.strength - 2

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
