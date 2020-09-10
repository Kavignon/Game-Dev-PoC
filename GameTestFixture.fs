module GameFixture

open System
open GameUnit
open TeamInventory
open GameStore
open CharacterInformation
open FsCheck

open Game.Dev.PoC

type GameTestFixture = {
    Party: GameUnit list
    Store: GameStore
    PartyInventory: Inventory
}

let random = Random()

let MinPartySize = 3
let MaxPartySize = 13

let stringGenerator = Arb.generate<string>
let int16Generator = Arb.generate<int16>
let characterJobGenerator = Arb.generate<CharacterJob>
let characterStateGenerator = Arb.generate<CharacterState>
let equipmentGenerator = Arb.generate<Equipment>
let consummableItemGenerator = Arb.generate<ConsumableItem>
let gameItemGenerator = Arb.generate<GameItem>

let createSingleValueWithGenerator<'Value> (generator: Gen<'Value'>) =
    generator |> Gen.sample 1 1 |> List.head

let unitPartyFixture =
    let generateGameUnit() =
        let equipmentListValues = [0 .. random.Next(1, EquipmentSlotLimit)]
        let consummableItemListValues = [0 .. random.Next(1, ConsummableItemSlotLimit)]
        {
            Name = createSingleValueWithGenerator<string> stringGenerator
            Lvl = createSingleValueWithGenerator<int16> int16Generator
            CurrentState = createSingleValueWithGenerator<CharacterState> characterStateGenerator
            TotalXp = random.Next(0, MaxExperiencePointBeforeLevelingUp)
            SkillPoints = random.Next(0, MaxSkillPoints)
            Job = createSingleValueWithGenerator<CharacterJob> characterJobGenerator
            Equipment = List.map(fun _ -> createSingleValueWithGenerator<Equipment> equipmentGenerator) equipmentListValues
            Consummables = List.map(fun _ -> createSingleValueWithGenerator<ConsumableItem> consummableItemGenerator) consummableItemListValues
        }

    let partySize = random.Next(MinPartySize, MaxPartySize)
    List.map(fun _ -> generateGameUnit()) [1..partySize]
    
let inventoryFixture =
    let itemCount = random.Next(2, StoreStockCapacity)
    let currentCapacity = [1 .. itemCount] 

    {
        Items = currentCapacity |> List.map(fun _ -> 
            (createSingleValueWithGenerator<GameItem> gameItemGenerator, random.Next(0, MaxDuplicateCount)))
        CurrentWeight = 50.00
        MaxWeight = 100.00
        Funds = 4000 |> int16
    }
    

let storeFixture =
    let itemCount = random.Next(2, StoreStockCapacity)
    let currentCapacity = [1 .. itemCount] 

    let storeInventory = {
        Items = currentCapacity |> List.map(fun _ -> 
            (createSingleValueWithGenerator<GameItem> gameItemGenerator, random.Next(0, MaxDuplicateCount)))
        Funds = 4000 |> int16
    }

    GameStore(storeInventory, 4000 |> int16)

let gameFixture = {
    Party = unitPartyFixture
    Store = storeFixture
    PartyInventory = inventoryFixture
}