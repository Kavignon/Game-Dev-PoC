module GameUnit

open Game.Dev.PoC
open CharacterInformation

let EquipmentSlotLimit = 6
let ConsummableItemSlotLimit = 10
let MaxExperiencePointBeforeLevelingUp = 900
let MaxSkillPoints = 20

type GameUnit = {
    Name: string
    Lvl: int16
    Job: CharacterJob
    CurrentState: CharacterState
    TotalXp: int 
    SkillPoints: int16
    Equipment: Equipment list
    Consummables: ConsumableItem list
}
with
    member x.CanPurchaseEquipment =
        x.Equipment.Length < 6
