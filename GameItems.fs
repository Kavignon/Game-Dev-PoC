type ConsumableItem =
    | HealthPotion
    | HighHealthPotion
    | MegaHealthPotion
    | Elixir
    | HighElixir
    | MegaElixir
    | PhoenixFeather
    | MedicinalHerb
    override x.ToString() =
        match x with
        | HealthPotion     -> "Health Potion"
        | HighHealthPotion  -> "High Health Potion"
        | MegaHealthPotion -> "Mega Health Potion"
        | Elixir            -> "Elixir"
        | HighElixir        -> "High Elixir"
        | MegaElixir        -> "Mega Elixir"
        | PhoenixFeather    -> "Phoenix Feather"
        | MedicinalHerb     -> "Medicinal Herb"

    member x.Name =
        match x with
        | _ -> x.ToString()

    member x.Weight =
        match x with
        | HealthPotion       -> 0.02
        | HighHealthPotion   -> 0.03
        | MegaHealthPotion   -> 0.05
        | Elixir             -> 0.02
        | HighElixir         -> 0.03
        | MegaElixir         -> 0.05
        | PhoenixFeather     -> 0.05
        | MedicinalHerb      -> 0.03

    member x.Price =
        match x with
        | HealthPotion       -> 100
        | HighHealthPotion   -> 150
        | MegaHealthPotion   -> 225
        | Elixir             -> 125
        | HighElixir         -> 175
        | MegaElixir         -> 275
        | PhoenixFeather     -> 350
        | MedicinalHerb      -> 50

type WeaponRank =
        | RankE
        | RankD
        | RankC
        | RankB
        | RankA
        | RankS
    with 
        member x.rankMultiplier = 
            match x with
            | RankE -> 1.0100
            | RankD -> 1.0375
            | RankC -> 1.0925
            | RankB -> 1.1250
            | RankA -> 1.1785
            | RankS -> 1.2105

type IStats =
    abstract member showStat : unit -> string

type WeaponStat = {
    Damage : float
    Intelligence : float
    Defense : float
    Speed : float
    Critical : float
    HitLimit : int
    Rank : WeaponRank
}
with
    interface IStats with
        member x.showStat() =
            let oIntelVal =
                match x.Intelligence with
                | Some v -> v
                | None -> 0.00

            sprintf "Weapon damage : %O - Intelligence : %O - Defense : %O - Speed : %O - Critical hit : %O - Weapon hit limit : %O - Weapon rank : %O"
                x.Damage oIntelVal x.Defense x.Speed x.Critical x.HitLimit x.Rank

type MagicalWeaponStat = {
    AttackRange : int
    Damage      : float
    Rank        : WeaponRank
    Uses        : int
    ManaCost    : float
}
with
    interface IStats with
        member x.showStat() =
            sprintf "Damage : %O - Attack range : %O - Mana cost: %O - Number of use: %O - Rank of spell: %O" x.Damage x.AttackRange x.ManaCost x.Uses x.Rank

type ItemDetails = {
    Weight: float
    Price: int 
}

type PhysicalWeaponType =
    | Dagger
    | Sword
    | Axe
    | Spear
    | Blade
    | Staff 

type MagicalWeaponType =
    | Spellbook
    // Could later add wands, amulets, etc.

type WeaponDetails =
    | PhysicalWeapon of PhysicalWeaponType * WeaponStat
    | MagicalWeapon of MagicalWeaponType * MagicalWeaponStat
        
type Weaponry = {
    Name: string
    ItemDetails: ItemDetails
    WeaponDetails: WeaponDetails 
}
with
    member x.Name = x.Name
    member x.Weight = x.ItemDetails.Weight
    member x.Price  = x.ItemDetails.Price
    member x.Stats  =
        match x.WeaponDetails with
        | PhysicalWeapon (_, stats) -> stats :> IStats
        | MagicalWeapon  (_, stats) -> stats :> IStats

module PhysicalWeapons =
    [<AutoOpen>]
    module Daggers =
        let rustedDagger = {
            Name = "Rusted dagger"
            ItemDetails = { Weight = 2.10; Price = 80 }
            WeaponDetails = PhysicalWeapon (Dagger, { Damage = 5.60; Defense = 1.20; Intelligence = None; Speed = 1.00; Critical = 0.02; HitLimit = 20; Rank = RankE })
        }

        let ironDagger = {
            Name = "Iron dagger"
            ItemDetails = { Weight = 2.80; Price = 200 }
            WeaponDetails = PhysicalWeapon (Dagger, { Damage = 9.80; Defense = 2.30; Intelligence = None; Speed = 1.10; Critical = 0.04; HitLimit = 25; Rank = RankD })
        }

        let steelDagger = {
            Name = "Steel dagger"
            ItemDetails = { Weight = 4.25; Price = 350 }
            WeaponDetails = PhysicalWeapon (Dagger, { Damage = 13.10; Defense = 3.00; Intelligence = None; Speed = 1.15; Critical = 0.05; HitLimit = 30; Rank = RankC })
        }

    [<AutoOpen>]
    module Swords =
        let brokenSword = {
            Name = "Broken sword"
            ItemDetails = { Weight = 7.20; Price = 90 }
            WeaponDetails = PhysicalWeapon (Sword, { Damage = 5.40; Defense = 2.50; Intelligence = None; Speed = 1.20; Critical = 0.01; HitLimit = 10; Rank = RankE })
        }

        let rustedSword = {
            Name = "Rusted sword"
            ItemDetails = { Weight = 8.50; Price = 120 }
            WeaponDetails = PhysicalWeapon (Sword, { Damage = 8.75; Defense = 2.90; Intelligence = None; Speed = 1.05; Critical = 0.03; HitLimit = 20; Rank = RankD })
        }

        let ironSword =  {
            Name = "Iron sword"
            ItemDetails = { Weight = 12.35; Price = 250 }
            WeaponDetails = PhysicalWeapon(Sword, { Damage = 11.1; Defense = 3.40; Intelligence = None ;Speed = 1.00; Critical = 0.04; HitLimit = 25; Rank = RankC })
        }

        let steelSword = { 
            Name = "Steel sword"
            ItemDetails = { Weight = 15.00; Price = 525}
            WeaponDetails = PhysicalWeapon(Sword, { Damage = 15.25; Defense = 4.30;Intelligence = None ; Speed = 0.85; Critical = 0.06; HitLimit = 35; Rank = RankB } )
        }

    [<AutoOpen>]
    module Axes = 
        let rustedAxe = {
            Name = "Rusted axe"
            ItemDetails = { Weight = 8.00; Price = 125 }
            WeaponDetails = PhysicalWeapon(Axe, { Damage = 7.20; Defense = 2.10; Speed = -1.00;  Intelligence = None ; Critical =0.03 ; HitLimit =   20; Rank = RankE } )
        }

        let ironAxe = {
            Name = "Iron axe"
            ItemDetails = { Weight = 10.00; Price = 280}
            WeaponDetails = PhysicalWeapon(Axe, { Damage = 11.80; Defense = 2.90; Speed = -1.50; Intelligence = None ; Critical = 0.06 ;  HitLimit = 25; Rank = RankD } )
        }

        let rustedBattleAxe = {
            Name = "Rusted battle axe"
            ItemDetails = { Weight = 9.00; Price = 150}
            WeaponDetails = PhysicalWeapon(Axe, { Damage = 7.10; Defense = 2.30; Speed = -1.20; Intelligence = None ; Critical = 0.04 ; HitLimit =  20; Rank = RankE } )
        }

        let ironBattleAxe = {
            Name = "Iron battle axe"
            ItemDetails = { Weight = 13.00; Price = 300}
            WeaponDetails = PhysicalWeapon(Axe, { Damage = 12.00; Defense = 3.05; Speed = -1.60;Intelligence = None ; Critical = 0.07 ; HitLimit =  25; Rank = RankD } )
        }

        let steelBattleAxe = {
            Name = "Steel battle axe"
            ItemDetails = { Weight = 16.00; Price = 425}
            WeaponDetails = PhysicalWeapon(Axe, { Damage = 16.20; Defense = 3.50; Speed = -2.60;Intelligence = None ; Critical = 0.095 ; HitLimit =  30; Rank = RankC })
        }

    [<AutoOpen>]
    module Spears = 
        let rustedSpear = {
            Name = "Rusted spear"
            ItemDetails = { Weight = 15.00; Price = 325}
            WeaponDetails = PhysicalWeapon(Spear, { Damage = 8.20; Intelligence = None; Defense = -3.30; Speed = -1.10; Critical = 0.05; HitLimit = 12; Rank = RankE } )
        }

        let ironSpear = {
            Name = "Iron spear"
            ItemDetails = { Weight = 20.00; Price = 325}
            WeaponDetails = PhysicalWeapon(Spear, { Damage = 12.00; Intelligence = None; Defense = -4.25; Speed = -1.50; Critical = 0.075; HitLimit = 15; Rank = RankD } )
        }

        let steelSpear = {
            Name = "Steel spear"
            ItemDetails = { Weight = 30.00; Price = 550}
            WeaponDetails = PhysicalWeapon(Spear, { Damage = 14.75; Intelligence = None; Defense = -5.05; Speed = -1.75; Critical = 0.0925; HitLimit = 20; Rank = RankC } )
        }

    [<AutoOpen>]
    module Blades = 
        let rustedLongBlade = {
            Name = "Rusted long blade"
            ItemDetails = { Weight = 6.00; Price = 120}
            WeaponDetails = PhysicalWeapon(Blade, { Damage = 6.00; Defense = 0.5; Intelligence = None; Speed = 1.10; Critical = 0.01; HitLimit = 10; Rank = RankE } )
        }

        let rustedKatana = {
            Name = "Rusted katana"
            ItemDetails = { Weight = 7.75; Price = 100}
            WeaponDetails = PhysicalWeapon(Blade, { Damage = 5.50; Defense = 0.45; Intelligence = None; Speed = 1.07; Critical = 0.02; HitLimit = 10; Rank = RankE })
        }

        let ironLongBlade = {
            Name = "Iron long blade"
            ItemDetails = { Weight = 14.25; Price = 215}
            WeaponDetails = PhysicalWeapon(Blade, { Damage = 8.50; Defense = 0.65; Intelligence = None; Speed = 1.20; Critical = 0.03; HitLimit = 10; Rank = RankD })
        }

        let curvedLongBlade = {
            Name = "Curved long blade"
            ItemDetails = { Weight = 11.20; Price = 240}
            WeaponDetails = PhysicalWeapon(Blade, { Damage = 7.00; Defense = 0.80; Intelligence = None; Speed = 1.25; Critical = 0.055; HitLimit = 10; Rank = RankD })
        }

        let steelKatana = {
            Name = "Steel katana"
            ItemDetails = { Weight = 16.78; Price = 350}
            WeaponDetails = PhysicalWeapon(Blade, { Damage = 13.0; Defense = 1.00; Intelligence = None; Speed = 1.60; Critical = 0.07; HitLimit = 15; Rank = RankC })
        }

        let steelLongBlade = {
            Name = "Steel long blade"
            ItemDetails = { Weight = 15.30; Price = 410}
            WeaponDetails = PhysicalWeapon(Blade, { Damage = 15.00; Defense = 1.10; Intelligence = None; Speed = 1.55; Critical = 0.085; HitLimit = 15; Rank = RankC })
        }

    [<AutoOpen>]
    module MagicalStaffs = 
        let rookieStaff = {
            Name = "Rookie staff"
            ItemDetails = { Weight = 2.20; Price = 180}
            WeaponDetails = PhysicalWeapon(Staff, { Damage = 3.00; Defense = 1.50; Intelligence = Some 4.00; Speed = 1.00; Critical = 0.02; HitLimit= 10; Rank = RankE })
        }

        let adeptStaff = {
            Name = "Adept staff"
            ItemDetails = { Weight = 4.20; Price = 270}
            WeaponDetails = PhysicalWeapon(Staff, { Damage = 5.00; Defense = 2.00; Intelligence = Some 7.00; Speed = 0.80; Critical = 0.045; HitLimit= 12; Rank = RankD })
        }

        let sorcererStaff = {
            Name = "Sorcerer staff"
            ItemDetails = { Weight = 5.10; Price = 445}
            WeaponDetails = PhysicalWeapon(Blade, { Damage = 15.00; Defense = 1.10; Intelligence = None; Speed = 1.55; Critical = 0.085; HitLimit = 15; Rank = RankC })
        }

        let necromancerStaff = {
            Name = "Necromancer staff"
            ItemDetails = { Weight = 3.20; Price = 550}
            WeaponDetails = PhysicalWeapon(Blade, { Damage = 15.00; Defense = 1.10; Intelligence = None; Speed = 1.55; Critical = 0.085; HitLimit = 15; Rank = RankC })
        }


    [<AutoOpen>]
    module MagicalWeapons = 
        let rank1SpellbookDetails = { Weight = 0.05; Price = 150 }
        let rank2SpellbookDetails = { Weight = 0.05; Price = 350 }

        let bookOfFireball = {
            Name = "Fireball"
            ItemDetails = rank1SpellbookDetails
            WeaponDetails = MagicalWeapon (Spellbook, { Damage = 8.0; AttackRange = 1; Rank = RankE; Uses = 30 ; ManaCost = 12.0<mp> })
        }

        let bookOfThunder = {
            Name = "Thunder"
            ItemDetails = rank1SpellbookDetails
            WeaponDetails = MagicalWeapon (Spellbook, { Damage = 8.0; AttackRange = 1; Rank = RankE; Uses = 30 ; ManaCost = 12.0<mp> })
        }

        let bookOfFrost= {
            Name = "Thunder"
            ItemDetails = rank1SpellbookDetails
            WeaponDetails = MagicalWeapon (Spellbook, { Damage = 8.0; AttackRange = 1; Rank = RankE; Uses = 30 ; ManaCost = 12.0<mp> })
        }

        let bookOfHellfire = {
            Name = "Hellfire"
            ItemDetails = rank2SpellbookDetails
            WeaponDetails = MagicalWeapon (Spellbook, { Damage = 6.50; AttackRange = 2; Rank = RankD; Uses = 25; ManaCost = 20.0<mp> })
        }

        let bookOfBlackFire = {
            Name = "Black fire"
            ItemDetails = rank2SpellbookDetails
            WeaponDetails = MagicalWeapon (Spellbook, { Damage = 11.2; AttackRange = 2; Rank = RankC; Uses = 20; ManaCost = 25.0<mp> })
        }

        let bookOfStormOfBlades = {
            Name = "Storm of blades"
            ItemDetails = rank2SpellbookDetails
            WeaponDetails = MagicalWeapon (Spellbook, { Damage = 5.80; AttackRange = 3; Rank = RankD; Uses = 30; ManaCost = 22.0<mp> })
        }
        
    let computeCharacterOverallOffensive (weapon: Weaponry) (cStats: CharacterStats) =
        let weaponDamage =
            match weapon.WeaponDetails with
            | PhysicalWeapon (_, stats) -> stats.Damage
            | MagicalWeapon  (_, stats) -> stats.Damage

        let weaponRank =
            match weapon.WeaponDetails with
            | PhysicalWeapon (_, stats) -> stats.Rank
            | MagicalWeapon  (_, stats) -> stats.Rank

        cStats.Strength * weaponRank.rankMultiplier * weaponDamage

[<AutoOpen>]
module CharacterWearableProtection =
    type CharacterProtectionStats = {
        Defense : float
        Resistance : float
        Intelligence : float option
        MagicResist : float
        Speed  : float
        EquipmentUsage : int
    }
    with
        interface IStats with
            member x.showStat() =
                sprintf "Defense : %O - Resistance : %O - Magic resistance : %O - Speed : %O - Equipment usage : %O" x.Defense x.Resistance x.MagicResist x.Speed x.EquipmentUsage

    type Hat =
        | SorcererHat
        | InfantryHat
        | JourneyManHelmet
        | RustedHelmet
        | MerlinsHat
        | IronHelmet
        | SteelHelmet
    with
        override x.ToString() =
            match x with
            | SorcererHat -> "Sorcerer hat"
            | InfantryHat -> "Infantry hat"
            | JourneyManHelmet -> "Journey man helmet"
            | RustedHelmet -> "Rusted helmet"
            | MerlinsHat -> "Merlin's hat"
            | IronHelmet -> "Iron helmet"
            | SteelHelmet -> "Steel helmet"

        member x.Name = match x with | _ -> x.ToString()

        member x.Price =
            match x with 
            | SorcererHat   -> 120     
            | InfantryHat   -> 70
            | JourneyManHelmet -> 180
            | RustedHelmet -> 95
            | MerlinsHat   -> 275
            | IronHelmet   -> 160
            | SteelHelmet  -> 325

        member x.Weight =
            match x with
            | SorcererHat -> 1.0
            | InfantryHat -> 3.20
            | JourneyManHelmet -> 2.25
            | RustedHelmet -> 2.50
            | MerlinsHat -> 4.10
            | IronHelmet -> 6.25
            | SteelHelmet -> 8.00

        member x.CharacterProtectionStats =
            match x with
            | SorcererHat       -> { Defense = 1.20; Resistance = 1.30; Intelligence = Some 3.00; MagicResist = 1.80; Speed = 1.00; EquipmentUsage = 100 }
            | InfantryHat  -> { Defense = 2.20; Resistance = 1.80; Intelligence = None; MagicResist = 0.80; Speed = 0.95; EquipmentUsage = 100 }
            | JourneyManHelmet -> { Defense = 5.60; Resistance = 3.20; Intelligence = None; MagicResist = 0.60; Speed = 0.90; EquipmentUsage = 100 }
            | RustedHelmet -> { Defense = 4.80; Resistance = 2.70; Intelligence = None; MagicResist = 0.20; Speed = 1.00; EquipmentUsage = 75 }
            | MerlinsHat -> { Defense = 3.10; Resistance = 1.70; Intelligence = Some 6.50; MagicResist = 3.45; Speed = 1.03; EquipmentUsage = 100 }
            | IronHelmet -> { Defense = 7.20; Resistance = 3.50; Intelligence = None; MagicResist = 0.40; Speed = 0.98; EquipmentUsage = 100 }
            | SteelHelmet -> { Defense = 10.80; Resistance = 4.20; Intelligence = None; MagicResist = 0.40; Speed = 0.95; EquipmentUsage = 100 }

    type Armor =
        | InfantryArmor
        | RustedArmor
        | IronArmor
        | SteelArmor
        | MyrtilleArmor
    with
        override x.ToString() =
            match x with
            | InfantryArmor -> "Infantry armor"
            | RustedArmor -> "Rusted armor"
            | IronArmor   -> "Iron armor"
            | SteelArmor  -> "Steel armor"
            | MyrtilleArmor -> "Myrtille armor"

        member x.Weight =
            match x with
            | InfantryArmor  -> 6.0
            | RustedArmor    -> 5.5
            | IronArmor      -> 10.0
            | SteelArmor     -> 15.0
            | MyrtilleArmor  -> 22.75

        member x.Price =
            match x with 
            | InfantryArmor -> 120 
            | RustedArmor -> 100 
            | IronArmor -> 220
            | SteelArmor -> 450 
            | MyrtilleArmor -> 790

        member x.Name = match x with | _ -> x.ToString()

        member x.CharacterProtectionStats =
            match x with
            | InfantryArmor -> { Defense = 6.50; Resistance = 2.20; Intelligence = None; MagicResist = 0.00; Speed = 0.99; EquipmentUsage = 100 }
            | RustedArmor   -> { Defense = 4.20; Resistance = 1.10; Intelligence = None; MagicResist = 0.30; Speed = 1.00; EquipmentUsage = 50 }
            | IronArmor     -> { Defense = 12.00; Resistance = 4.30; Intelligence = None; MagicResist = 1.00; Speed = 0.975; EquipmentUsage = 100 }
            | SteelArmor    -> { Defense = 17.40; Resistance = 6.10; Intelligence = None; MagicResist = 2.30; Speed = 0.945; EquipmentUsage = 100 }
            | MyrtilleArmor -> { Defense = 23.55; Resistance = 10.10; Intelligence = None; MagicResist = 5.70; Speed = 0.9725; EquipmentUsage = 100 }

    type Pants =
        | InfantryPants
        | IronPants
        | SteelPants
        | MyrtillePants
    with
        override x.ToString() =
            match x with
            | InfantryPants -> "Infantry pants"
            | IronPants -> "Iron pants"
            | SteelPants -> "Steel pants"
            | MyrtillePants -> "Myrtille pants"

        member x.Name = match x with | _ -> x.ToString()

        member x.Price =
            match x with 
            | InfantryPants -> 50 
            | IronPants -> 140 
            | SteelPants -> 190 
            | MyrtillePants -> 400 

        member x.Weight =
            match x with
            | InfantryPants -> 4.00
            | IronPants -> 5.75
            | SteelPants -> 10.80
            | MyrtillePants -> 15.20

        member x.CharacterProtectionStats =
            match x with
            | InfantryPants ->
                { Defense = 0.85; Resistance = 0.30; Intelligence = None; MagicResist = 0.00; Speed = 0.99; EquipmentUsage = 100 }
            | IronPants -> { Defense = 3.50; Resistance = 1.20; Intelligence = None; MagicResist = 0.50; Speed = 0.98; EquipmentUsage = 100 }
            | SteelPants -> { Defense = 5.10; Resistance = 2.00; Intelligence = None; MagicResist = 1.00; Speed = 0.95; EquipmentUsage = 100 }
            | MyrtillePants -> { Defense = 6.50; Resistance = 2.70; Intelligence = None; MagicResist = 3.00; Speed = 0.9765; EquipmentUsage = 100 }

    type Gauntlets =
        | InfantryGauntless
        | RustedGauntless
        | IronGauntless
        | SteelGauntless
    with
        override x.ToString() =
            match x with
            | InfantryGauntless -> "Infantry gauntlets"
            | RustedGauntless -> "Rusted gauntlets"
            | IronGauntless -> "Iron gauntlets"
            | SteelGauntless -> "Steel gauntlets"

        member x.Price = 
            match x with 
            | InfantryGauntless -> 40 
            | RustedGauntless -> 25 
            | IronGauntless -> 95 
            | SteelGauntless -> 185 

        member x.Name = match x with | _ -> x.ToString()

        member x.Weight =
            match x with
            | InfantryGauntless -> 1.00
            | RustedGauntless -> 1.70
            | IronGauntless -> 3.20
            | SteelGauntless -> 6.55

        member x.CharacterProtectionStats =
            match x with
            | InfantryGauntless ->
                { Defense = 0.85; Resistance = 0.15; Intelligence = None; MagicResist = 0.10; Speed = 0.995; EquipmentUsage = 100 }
            | RustedGauntless ->
                { Defense = 1.2; Resistance = 0.45; Intelligence = None; MagicResist = 0.20; Speed = 0.99; EquipmentUsage = 50 }
            | IronGauntless ->
                { Defense = 3.65; Resistance = 0.85; Intelligence = None; MagicResist = 0.25; Speed = 0.975; EquipmentUsage = 100 }
            | SteelGauntless ->
                { Defense = 5.15; Resistance = 1.35; Intelligence = None; MagicResist = 0.45; Speed = 0.95; EquipmentUsage = 100 }

    type RingStats = {
        ExtraStrength : float<str> option
        ExtraDamage   : float<dmg> option
        ExtraHealth   : float<hp> option
        ExtraMana     : float<mp> option
    }
    with
        interface IStats with
            member x.showStat() =
                sprintf ""
        static member Initial =
            { ExtraDamage = None; ExtraStrength = None; ExtraHealth = None; ExtraMana = None }

    type Ring =
        | ExtraStrenghtRing
        | ExtraDamageRing
        | ExtraHealthRing
        | ExtraManaRing
    with
        override x.ToString() =
            match x with
            | ExtraStrenghtRing -> "Extra strenght ring"
            | ExtraDamageRing -> "Extra damage ring"
            | ExtraHealthRing -> "Extra health ring"
            | ExtraManaRing -> "Extra mana ring"

        member x.Name = match x with | _ -> x.ToString()

        member x.Price = 
            match x with 
            | _ -> 275

        member x.Weight =
            match x with
            | _ -> 0.75

        member x.CharacterProtectionStat =
            match x with
            | ExtraDamageRing ->  { RingStats.Initial with ExtraDamage = Some 5.00<dmg> }
            | ExtraStrenghtRing -> { RingStats.Initial with ExtraStrength = Some 4.50<str> }
            | ExtraHealthRing -> { RingStats.Initial with ExtraHealth = Some 12.00<hp> }
            | ExtraManaRing -> { RingStats.Initial with ExtraMana = Some 7.00<mp> }

    type ShieldStats = {
        Defense : float
        Resistance :float
        Speed   : float
        MagicResist : float
        ShieldRank  : WeaponRank
        EquipmentUsage : int
    }
    with
        interface IStats with
            member x.showStat() =
                sprintf "Defense : %O - Resistance : %O - Magic resistance : %O - Equipment usage : %O" x.Defense x.Resistance x.MagicResist x.EquipmentUsage

    type Shield =
        | RustedShield
        | SmallShield
        | KnightShield
        | HeavyShield
        | SteelShield
    with
        override x.ToString()  =
            match x with
            | RustedShield -> "Rusted shield"
            | SmallShield -> "Small shield"
            | KnightShield -> "Knight shield"
            | HeavyShield -> "Heavy shield"
            | SteelShield -> "Steel shield"

        member x.Name = match x with | _ -> x.ToString()

        member x.Price = 
            match x with 
            | RustedShield -> 100 
            | SmallShield -> 75 
            | KnightShield -> 215 
            | HeavyShield -> 425 
            | SteelShield -> 500

        member x.Weight =
            match x with
            | RustedShield -> 5.00
            | SmallShield -> 3.25
            | KnightShield -> 6.80
            | HeavyShield -> 12.50
            | SteelShield -> 10.80

        member x.CharacterProtectionStat =
            match x with
            | RustedShield ->
                { Defense = 5.60; Resistance = 3.10; Speed = 0.96; MagicResist = 1.85; ShieldRank = RankE; EquipmentUsage = 50 }
            | SmallShield ->
                { Defense = 3.80; Resistance = 2.70; Speed = 0.985; MagicResist = 0.50; ShieldRank = RankE; EquipmentUsage = 100 }
            | KnightShield ->
                { Defense = 9.80; Resistance = 5.10; Speed = 0.94; MagicResist = 3.40; ShieldRank = RankC; EquipmentUsage = 100 }
            | HeavyShield ->
                { Defense = 12.75; Resistance = 6.40; Speed = 0.88; MagicResist = 4.60; ShieldRank = RankC; EquipmentUsage = 100 }
            | SteelShield ->
                { Defense = 17.20; Resistance = 9.30; Speed = 0.93; MagicResist = 5.90; ShieldRank = RankB; EquipmentUsage = 100 }

    type CharacterProtection =
        | Shield        of Shield
        | Ring          of Ring
        | Gloves        of Gauntlets
        | Legs          of Pants
        | Armor         of Armor
        | Helmet        of Hat

    with
        member x.Name =
            match x with
            | Shield s -> s.Name
            | Ring r -> r.Name
            | Gloves g -> g.Name
            | Legs l -> l.Name
            | Armor a -> a.Name
            | Helmet h -> h.Name

        member x.Price =
            match x with 
            | Shield s -> s.Price 
            | Ring r -> r.Price
            | Gloves g -> g.Price
            | Legs l -> l.Price
            | Armor a -> a.Price 
            | Helmet a -> a.Price

        member x.Weight =
            match x with
            | Shield s -> s.Weight
            | Ring r -> r.Weight
            | Gloves g -> g.Weight
            | Legs l -> l.Weight
            | Armor a -> a.Weight
            | Helmet h -> h.Weight

        member x.ProtectionStats =
            match x with
            | Shield s -> s.CharacterProtectionStat :> IStats
            | Ring r -> r.CharacterProtectionStat :> IStats
            | Gloves g -> g.CharacterProtectionStats :> IStats
            | Legs l -> l.CharacterProtectionStats :> IStats
            | Armor a -> a.CharacterProtectionStats :> IStats
            | Helmet h -> h.CharacterProtectionStats   :> IStats
    
    [<AutoOpen>]
    module GameItems =
        type Equipment =
            | Weapon of Weaponry
            | Armor of CharacterProtection
        with
            member x.Weight =
                match x with
                | Weapon w -> w.Weight
                | Armor a -> a.Weight
            
            member x.Name =
                match x with
                | Weapon w -> w.Name
                | Armor a -> a.Name

            member x.Price =
                match x with
                | Weapon w -> w.Price
                | Armor a -> a.Price

        type GameItem =
            | Consumable of ConsumableItem 
            | Equipement of Equipment 
        with 
            member x.Weight = 
                match x with 
                | Consumable c -> c.Weight 
                | Equipement e -> e.Weight

            member x.Name = 
                match x with
                | Consumable c -> c.Name 
                | Equipement w -> e.Name

            member x.Price = 
                match x with 
                | Consumable c -> c.Price 
                | Equipement e -> e.Price