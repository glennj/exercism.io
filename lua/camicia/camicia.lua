local VALUES = {A = 4, K = 3, Q = 2, J = 1}

local function value(card)
    return VALUES[card] or 0
end

local function is_payment_card(card)
    return value(card) > 0
end

local function hand_state(hand)
    local values = {}
    for i, c in ipairs(hand) do
        values[i] = value(c)
    end
    return table.concat(values)
end

local function simulate_game(playerA, playerB)
    local hands = {A = playerA, B = playerB}
    local seen, pile, cards, tricks = {}, {}, 0, 0
    local current, other = 'A', 'B'

    -- -------------------------------------------------------
    -- some closures
    local function is_empty(player)
        return #hands[player] == 0
    end

    local function collect_trick(player)
        for _, card in ipairs(pile) do
            table.insert(hands[player], card)
        end
        pile = {}
        tricks = tricks + 1
    end

    local function discard(player)
        local card = table.remove(hands[player], 1)
        table.insert(pile, card)
        cards = cards + 1
        return card
    end

    local function pay_penalty(card, payer, payee)
        for i = value(card), 1, -1 do
            if is_empty(payer) then
                break
            end
            local c = discard(payer)
            if is_payment_card(c) then
                return pay_penalty(c, payee, payer)
            end
        end

        -- penalty is fully paid: return next current and other
        collect_trick(payee)
        return payee, payer
    end

    local function result(status)
        return {status = status, cards = cards, tricks = tricks}
    end

    -- -------------------------------------------------------
    -- the game play
    while true do
        local state = hand_state(hands.A) .. ':' .. hand_state(hands.B)
        if seen[state] then
            return result('loop')
        end
        seen[state] = true

        if is_empty(current) then
            collect_trick(other)
            return result('finished')
        end

        local card = discard(current)

        if is_payment_card(card) then
            current, other = pay_penalty(card, other, current)
            if is_empty(other) then
                return result('finished')
            end
        else
            -- next player's turn
            current, other = other, current
        end
    end
end

return { simulate_game = simulate_game }
