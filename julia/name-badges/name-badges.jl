function print_name_badge(id, name, department)
    dept = isnothing(department) ? "OWNER" : uppercase(department)
    label = ismissing(id) ? "" : "[$id] - "
    label * "$name - $dept"
end

function salaries_no_id(ids, salaries)
    sum([salary for (id, salary) in zip(ids, salaries) if ismissing(id)], init=0)
end
