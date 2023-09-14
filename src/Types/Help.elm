module Types.Help exposing (..)

helpInfo : List String
helpInfo =
    [ "Classes:"
    , "Rogue -> Higher base Dexterity, improved Stealth"
    , "Spy -> Higher base Charisma, improved Taunt"
    , "Warrior -> Higher base Strength, improved Furious Attack"
    , "Tank -> Higher base Constitution, improved Heal"
    , "---------------------"
    , "Attributes:"
    , "Dexterity -> Increases initiative and damage of some weapons"
    , "Charisma -> Increases chance of avoiding trouble and improves success rate of Taunt"
    , "Strength -> Increases damage of some weapons"
    , "Constitution -> Increases health"
    , "---------------------"
    , "CHARACTER stats:"
    , "HP -> If it reaches 0, you die"
    , "Sanity -> Affects chances and quality of special actions."
    , "- Sanity decreases after each ENEMY kill"
    , "- Each turn Sanity is less than 10% PLAYER loses health"
    , "Rush -> Gives the character a free turn"
    , "Coins -> Money used to buy items at the end of each level"
    , "Turn Initiative -> Affects who acts first"
    , "---------------------"
    , "Actions: *Note: A PLAYER will lose their weapon until the end of the room if they perform a ranged attack"
    , "Ranged Attack -> Attacks an enemy from a distance"
    , "Melee Attack -> Attacks an enemy in melee range"
    , "Move Towards Enemy -> Move into melee range of an enemy"
    , "Push Enemy Away -> Push an enemy to a ranged distance"
    , "Special Actions:"
    , "Taunt -> Chance to make an enemy do half damage each turn for a number of turns"
    , "Furious Attack -> Attacks an enemy with a chance of doing double damage"
    , "Stealth -> Chance to make an enemy do half damage each turn for a number of turns"
    , "Heal -> Heal based off a percentage of your sanity"
    , "---------------------"
    , "ENEMY Actions:"
    , "The enemy will perform one of the following actions on each of their turns, depending on the situation and their weapon"
    , "Ranged Attack -> Attack PLAYER from a distance"
    , "Melee Attack -> Attack PLAYER in melee range"
    , "Move Towards Player -> Move into melee range of PLAYER"
    , "Push Player Away -> Push PLAYER to a ranged distance"
    , "Taunt -> Decrease PLAYER's sanity"
    , "---------------------"
    , "Setup:"
    , "The PLAYER is tasked with going through multiple rooms in each of multiple levels"
    , "The last room in each level contains a boss"
    , "The final level contains the final boss"
    , "---------------------"
    , "Between Rooms:"
    , "PLAYER may chose one of three actions in between each room on a level"
    , "Rest -> Heal 5 hp and 5% sanity"
    , "Loot -> Gain a number of coins for each enemy killed"
    , "Rush -> Gain 1 rush for the next room"
    , "---------------------"
    , "Between Levels:"
    , "PLAYER gets three more attribute points to spend"
    , "PLAYER may also purchase items from the vending machine"
    , "Water (2 coins) -> Heal 5 hp"
    , "Juice (5 coins) -> Recover 10% sanity"
    , "Hot Chocolate (10 coins) -> Fully heal and recover hp and sanity"
    , "Protein Shake (20 coins) -> Gain 10 permanent max health"
    ]