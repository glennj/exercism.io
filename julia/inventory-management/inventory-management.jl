create_inventory(items) = add_items(Dict(), items)

function add_items(inventory, items)
    for item in items
        inventory[item] = get(inventory, item, 0) + 1
    end
    inventory
end

function decrement_items(inventory, items)
    for item in items
        if haskey(inventory, item) && inventory[item] > 0
            inventory[item] -= 1
        end
    end
    inventory
end

remove_item = delete!
 
list_inventory(inventory) = filter!(p -> p.second > 0, inventory) |> collect |> sort
