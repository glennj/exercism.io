# If the knight is sleeping, then Annalyn will be able to make a quick attack
function can_do_fast_attack(knight_awake)
    return !knight_awake
end

# The group can be spied upon if at least one of them is awake. 
function can_spy(knight_awake, archer_awake, prisoner_awake)
    return knight_awake || archer_awake || prisoner_awake
end

# The prisoner can be signalled if she is awake and the archer is sleeping.
function can_signal_prisoner(archer_awake, prisoner_awake)
    return prisoner_awake && !archer_awake
end

#=
This is a risky thing to do and can only succeed in one of two ways:

If Annalyn has her pet dog with her, she can rescue the prisoner if the archer is asleep. 

If Annalyn does not have her dog then she and the prisoner must be very sneaky! Annalyn can free the prisoner if she is awake and the knight and archer are both sleeping, but if the prisoner is sleeping, she can't be rescued: 
=#
function can_free_prisoner(knight_awake, archer_awake, prisoner_awake, dog_present)
    return !archer_awake && (dog_present || (prisoner_awake && !knight_awake))
end
