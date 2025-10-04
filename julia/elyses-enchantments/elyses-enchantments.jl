get_item = getindex
set_item!(stack, pos, card) = setindex!(stack, card, pos)

insert_item_at_top! = push!
insert_item_at_bottom! = pushfirst!

remove_item! = deleteat!
remove_item_from_top!(stack) = remove_item!(stack, length(stack))
remove_item_at_bottom!(stack) = remove_item!(stack, 1)

check_size_of_stack(stack, stack_size) = length(stack) == stack_size