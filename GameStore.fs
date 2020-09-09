module GameStore

open GameUnit.Dev.PoC
open TeamInventory
open GameUnit

type PurchaseFromStore = (int16 * GameItem) -> Inventory -> GameUnit -> (Inventory * GameUnit)
type SellToStore = (int16 * GameItem) -> Inventory -> GameUnit -> (Inventory * GameUnit)

type GameStore (inventory: Inventory, funds: int16) =
    let mutable storeInventory = inventory
    let mutable storeFunds = funds

    let purchaseFromStore : PurchaseFromStore =
        fun itemAndQty inventory gameUnit ->
            let quantityInt = (fst itemAndQty) |> int
            let itemToBePurchased = (snd itemAndQty)

            if gameUnit.CanPurchaseEquipment <> true then
                (inventory, gameUnit)

            elif (quantityInt + gameUnit.Equipment.Length) < EquipmentSlotLimit then
                (inventory, gameUnit)

            elif (quantityInt |> float) * itemToBePurchased.Weight + inventory.CurrentWeight < inventory.MaxWeight then
                (inventory, gameUnit)

            else
                storeFunds <- storeFunds + (fst itemAndQty)

                let totalPurchaseOrder = itemToBePurchased.Price * quantityInt
                let addedWeight = itemToBePurchased.Weight * (quantityInt |> float)
                let gameUnit =
                    match itemToBePurchased with
                    | Consumable consummable ->
                        let storeItems = [1..quantityInt] |> List.map(fun _ -> consummable)

                        { gameUnit with Consummables = gameUnit.Consummables |> List.append storeItems }

                    | Equipement equipment ->
                        match equipment with 
                        | Weapon weapon ->
                            let storeItems = [1..quantityInt] |> List.map(fun _ -> Weapon weapon)

                            { gameUnit with Equipment = gameUnit.Equipment |> List.append storeItems }

                        | Armor armor ->
                            let storeItems = [1..quantityInt] |> List.map(fun _ -> Armor armor)

                            { gameUnit with Equipment = gameUnit.Equipment |> List.append storeItems }

                let inventory = {
                    inventory with 
                        Funds = inventory.Funds - (totalPurchaseOrder |> int16)
                        CurrentWeight = inventory.CurrentWeight + addedWeight
                }

                (inventory, gameUnit)

                

    member val Stock = storeInventory with get, set
    member val Fund = storeFunds with get, set

    member x.PurchaseFromStore = purchaseFromStore
    // TODO: Implement SellToStore object member.