local change = require('change')

describe('change', function()
  it('should generate the correct change when there is only one type of coin', function()
    assert.same({ 5 }, change(5, { 1 }))
  end)

  it('should generate the correct change when there are multiple coin types', function()
    assert.same({ 5, 0 }, change(5, { 1, 10 }))
  end)

  it('should generate the correct change when multiple types of coins are needed', function()
    assert.same({ 3, 1, 1 }, change(18, { 1, 5, 10 }))
  end)

  it('should return nil if it is not possible to make change', function()
    assert.is_nil(change(3, { 5, 10, 25 }))
  end)

  it('should generate the correct change given any coin order', function()
    assert.same({ 3, 1, 1 }, change(18, { 1, 5, 10 }))
    assert.same({ 1, 1, 3 }, change(18, { 10, 5, 1 }))
  end)

  it('should generate the correct change for large values with many coins', function()
    assert.same({ 3, 1, 0, 1, 1 }, change(133, { 1, 5, 10, 25, 100 }))
  end)

  -- these taken from the javascript test suite:

  it('test change with Lilliputian Coins where a greedy algorithm fails', function()
    -- https://en.wikipedia.org/wiki/Change-making_problem#Greedy_method
    assert.same({ 0, 2, 1, 0, 0 }, change(23, { 1, 4, 15, 20, 50 }))
  end);

  it('test change with Lower Elbonia Coins where a greedy algorithm fails', function()
    -- https://en.wikipedia.org/wiki/Change-making_problem#Greedy_method
    assert.same({ 0, 0, 0, 3, 0 }, change(63, { 1, 5, 10, 21, 25 }))
  end);

end)
