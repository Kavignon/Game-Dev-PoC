module GameDomain

[<Measure>] type gold
[<Measure>] type dmg // damage
[<Measure>] type ctr // critical
[<Measure>] type hl // hit limit
[<Measure>] type kg // weight in kilograms

type HeroClass =
    | Archer
    | Assassin
    | Knight
    | Sorcerer
    | Swordsman

type ConsumableItem =
    | HealthPotion
    | Elixir
    | PhoenixFeather
    | MedicinalHerb

type CombatStyle = 
    | BladeMaster
    | DualWielder of int
    | MaceUser of int 
    | Archer of int 
with 
    member x.actionPoints = 
        match x with 
        | DualWielder ap -> ap 
        | MaceUser ap -> ap
        | Archer ap -> ap 

[<Struct>]
type Dimension = {
    Lenght: float
    Width: float
    Height: float
}

type ItemDetails = { 
    Weight: float<kg>
    Price: int<gold> 
}

[<Struct>]
type WeaponRank =
    | RankB
    | RankA
    | RankS

[<Struct>]
type WeaponStat = {
    Damage : float<dmg>
    CriticalHitProbability : float<ctr>
    Durability : int<hl>
    Rank : WeaponRank
}

type Weapon = {
    Name: string
    Stats: WeaponStat
    Details: ItemDetails
    Dimensions: Dimension
    CombatStyle: CombatStyle
}

[<Struct>]
type GameTreasure = 
    | Health of health: ConsumableItem
    | Mana of mana: ConsumableItem 
    | MagicFeather of feather: ConsumableItem
    | Weapon of weapon: Weapon
    | Currency of currency: float<gold>


// Currently, the object GameHero is immutable
// All class members are public by default contrarly to C# (private)
// Let bindings in a class are private and cannot be made public
// If you are defining classes that need to interop with other .NET code, do not define them inside a module! Define them in a namespace instead
// F# modules are exposed as static classes
// and any F# classes defined inside a module are then defined as nested classes within the static class, which can mess up your interop

type GameHero(name: string, heroClass: HeroClass, consummables: ConsumableItem list, collectedTreasures: GameTreasure array, weapons: Weapon list) =
    let mutable consummables = consummables // consummables can now be mutated in the immutable type.
    
    member _.Name = name
    member _.Class = heroClass
    member _.CollectedTreasures = collectedTreasures
    member _.WeaponStash = weapons
    
    // mutable auto property
    member val CurrentConsummables = consummables with get, set

    member _.AddConsummable consummable = consummables <- consummable :: consummables 