module GameUnit

open GameUnit.Dev.PoC
open CharacterInformation

let EquipmentSlotLimit = 6

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
