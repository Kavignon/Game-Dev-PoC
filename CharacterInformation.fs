module CharacterInformation

type CharacterRole = 
    | Wizard of moveRange: int
    | Knight of moverange: int 
    | Fighter of moveRange: int
    | MagicSoldier of moveRange: int 
    | Sniper  of moveRange: int
with    
    member x.MoveRange = 
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

type RoleRank =
    | Beginner
    | Intermediate
    | Advanced
    | Master

type CharacterJob = 
    | Healer        of RoleRank * CharacterRole  * CharacterStats
    | Knight        of RoleRank * CharacterRole  * CharacterStats
    | Berserker     of RoleRank * CharacterRole  * CharacterStats
    | Rider         of RoleRank * CharacterRole  * CharacterStats
    | Paladin       of RoleRank * CharacterRole  * CharacterStats
    | BowAndBlade   of RoleRank * CharacterRole  * CharacterStats
    | Necromancer   of RoleRank * CharacterRole  * CharacterStats
with 
    member x.Stats = 
        match x with
        | Healer       (_, _,stats) -> stats
        | Knight       (_, _,stats) -> stats
        | Berserker    (_, _,stats) -> stats
        | Rider        (_, _,stats) -> stats
        | Paladin      (_, _,stats) -> stats
        | BowAndBlade  (_, _,stats) -> stats
        | Necromancer  (_, _,stats) -> stats

    member x.Role = 
        match x with
        | Healer       (_, role, _) -> role
        | Knight       (_, role, _) -> role
        | Berserker    (_, role, _) -> role
        | Rider        (_, role, _) -> role
        | Paladin      (_, role, _) -> role
        | BowAndBlade  (_, role, _) -> role
        | Necromancer  (_, role, _) -> role

type CharacterState = 
    | Healthy 
    | Poisoned 
    | Burnt 
    | Frozen
    | Paralyzed
    | Dead