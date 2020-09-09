type CharacterRole = 
    | Wizard of moveRange: int
    | Knight of moverange: int 
    | Fighter of moveRange: int
    | MagicSoldier of moveRange: int 
    | Sniper  of moveRange: int
with    
    member x.moveRange = 
        match x with 
        | Wizard mr -> mr 
        | Knight mr -> mr 
        | Fighter mr -> mr 
        | MagicSoldier mr -> mr 
        | Sniper  mr -> mr

type CharacterStats = {
    PhysicalStrength: float
    MagicalStrength: float
    PhysicalDefense: float
    MagicalDefense: float
    Evasiness: float
    CurrentHealth: int
    MaxHealth: int
    Weight: float
}

type CharacterJob = 
    | Healer        of CharacterRole  * CharacterStats
    | Knight        of CharacterRole  * CharacterStats
    | Berserker     of CharacterRole  * CharacterStats
    | Rider         of CharacterRole  * CharacterStats
    | Paladin       of CharacterRole  * CharacterStats
    | BowAndBlade   of CharacterRole  * CharacterStats
    | Necromancer   of CharacterRole  * CharacterStats
with 
    member x.Stats = 
        match x with
        | Healer       (_,stats) -> stats
        | Knight       (_,stats) -> stats
        | Berserker    (_,stats) -> stats
        | Rider        (_,stats) -> stats
        | Paladin      (_,stats) -> stats
        | BowAndBlade  (_,stats) -> stats
        | Necromancer  (_,stats) -> stats

    member x.Role = 
        match x with
        | Healer       (role, _) -> role
        | Knight       (role, _) -> role
        | Berserker    (role, _) -> role
        | Rider        (role, _) -> role
        | Paladin      (role, _) -> role
        | BowAndBlade  (role, _) -> role
        | Necromancer  (role, _) -> role

type CharacterState = 
    | Healthy 
    | Poisoned 
    | Burnt 
    | Frozen
    | Paralyzed
    | Dead

type CharacterUnit = {
    Name: string
    Lvl: uint
    Job: CharacterJob
    CurrentState: CharacterState
    TotalXp: int 
    SkillPoints: uint
    Equipment: Equipment
}