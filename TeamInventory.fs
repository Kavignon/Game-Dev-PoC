module TeamInventory

open Game.Dev.PoC

type Inventory = {
    Items: GameItem * int list
    CurrentWeight: float
    MaxWeight: float
    Funds: int16
}