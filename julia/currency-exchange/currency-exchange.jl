exchange_money(budget, exchange_rate) = budget / exchange_rate

get_change(budget, exchanging_value) = budget - exchanging_value

get_value_of_bills(denomination, number_of_bills) = denomination * number_of_bills

get_number_of_bills(amount, denomination) = amount ÷ denomination

function get_leftover_of_bills(amount, denomination)
    get_change(amount, get_value_of_bills(denomination, get_number_of_bills(amount, denomination)))
end

function exchangeable_value(budget, exchange_rate, spread, denomination)
    exch = exchange_rate * (1 + spread / 100)
    exchanged = exchange_money(budget, exch)
    num = get_number_of_bills(exchanged, denomination)
    get_value_of_bills(denomination, num)    
end
