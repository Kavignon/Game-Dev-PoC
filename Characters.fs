module GameUnit

open System
open CharacterInformation

type CharacterUnit = {
    Name: string
    Lvl: uint
    Job: CharacterJob
    CurrentState: CharacterState
    TotalXp: int 
    SkillPoints: uint
    Equipment: Equipment
}