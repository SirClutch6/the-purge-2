module Types.Weapons exposing (..)

import Types.Player as Player
import Types.Enemy as Enemy
import Logic.RandomGen as RNG

import Random

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

getWeaponDamage : Character -> Weapon -> AttackType -> Random.Seed -> (Int, Random.Seed)
getWeaponDamage character weapon range seed=
    let
        (damage, new_seed) =
            calcWeaponDamage character weapon range seed
    in
    if damage < 0 then
        (0, new_seed)
    else
        (damage, new_seed)
        
calcWeaponDamage : Character -> Weapon -> AttackType -> Random.Seed -> (Int, Random.Seed)
calcWeaponDamage character weapon range seed =
    let
        d = case character of
            Player player ->
                player.dexterity

            Enemy enemy ->
                enemy.dexterity
        (dex, new_seed) = Random.step (RNG.oneToCustom d) seed

        s = case character of
            Player player ->
                player.strength

            Enemy enemy ->
                enemy.strength
        (str, new_new_seed) = Random.step (RNG.oneToCustom s) new_seed

        calculated_damage =
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
                            str + 0

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
        in
        (calculated_damage, new_new_seed)

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
