Poker = require 'poker'

describe 'poker:', ->
  -- ----------------------------------------------------------
  -- assert that two lists have the same elements, regardless of order
  same_elements = (state, arguments) ->
    { list1, list2 } = arguments
    return false if #list1 != #list2
    for elem in *list1
      found = false
      for elem2 in *list2
        if elem == elem2
          found = true
          break
      return false if not found
    true

  say = require 'say'
  say\set 'assertion.same_elements.positive', 'Expected %s to be in the range [%s, %s]'
  say\set 'assertion.same_elements.negative', 'Expected %s not to be in the range [%s, %s]'
  assert\register 'assertion', 'same_elements', same_elements, 'assertion.same_elements.positive', 'assertion.same_elements.negative'
  -- ----------------------------------------------------------

  it 'single hand always wins', ->
    hands = {'4S 5S 7H 8D JC'}
    expected = {'4S 5S 7H 8D JC'}
    assert.are.same expected, Poker.bestHands hands

  it 'highest card out of all hands wins', ->
    hands = {
      '4D 5S 6S 8D 3C',
      '2S 4C 7S 9H 10H',
      '3S 4S 5D 6H JH',
    }
    expected = {'3S 4S 5D 6H JH'}
    assert.are.same expected, Poker.bestHands hands

  it 'a tie has multiple winners', ->
    hands = {
      '4D 5S 6S 8D 3C',
      '2S 4C 7S 9H 10H',
      '3S 4S 5D 6H JH',
      '3H 4H 5C 6C JD',
    }
    expected = {
      '3S 4S 5D 6H JH',
      '3H 4H 5C 6C JD',
    }
    assert.has.same_elements expected, Poker.bestHands hands

  it 'multiple hands with the same high cards, tie compares next highest ranked, down to last card', ->
    hands = {
      '3S 5H 6S 8D 7H',
      '2S 5D 6D 8C 7S',
    }
    expected = {'3S 5H 6S 8D 7H'}
    assert.are.same expected, Poker.bestHands hands

  it 'winning high card hand also has the lowest card', ->
    hands = {
      '2S 5H 6S 8D 7H',
      '3S 4D 6D 8C 7S',
    }
    expected = {'2S 5H 6S 8D 7H'}
    assert.are.same expected, Poker.bestHands hands

  it 'one pair beats high card', ->
    hands = {
      '4S 5H 6C 8D KH',
      '2S 4H 6S 4D JH',
    }
    expected = {'2S 4H 6S 4D JH'}
    assert.are.same expected, Poker.bestHands hands

  it 'highest pair wins', ->
    hands = {
      '4S 2H 6S 2D JH',
      '2S 4H 6C 4D JD',
    }
    expected = {'2S 4H 6C 4D JD'}
    assert.are.same expected, Poker.bestHands hands

  it 'both hands have the same pair, high card wins', ->
    hands = {
      '4H 4S AH JC 3D',
      '4C 4D AS 5D 6C',
    }
    expected = {'4H 4S AH JC 3D'}
    assert.are.same expected, Poker.bestHands hands

  it 'two pairs beats one pair', ->
    hands = {
      '2S 8H 6S 8D JH',
      '4S 5H 4C 8C 5C',
    }
    expected = {'4S 5H 4C 8C 5C'}
    assert.are.same expected, Poker.bestHands hands

  it 'both hands have two pairs, highest ranked pair wins', ->
    hands = {
      '2S 8H 2D 8D 3H',
      '4S 5H 4C 8S 5D',
    }
    expected = {'2S 8H 2D 8D 3H'}
    assert.are.same expected, Poker.bestHands hands

  it 'both hands have two pairs, with the same highest ranked pair, tie goes to low pair', ->
    hands = {
      '2S QS 2C QD JH',
      'JD QH JS 8D QC',
    }
    expected = {'JD QH JS 8D QC'}
    assert.are.same expected, Poker.bestHands hands

  it 'both hands have two identically ranked pairs, tie goes to remaining card (kicker)', ->
    hands = {
      'JD QH JS 8D QC',
      'JS QS JC 2D QD',
    }
    expected = {'JD QH JS 8D QC'}
    assert.are.same expected, Poker.bestHands hands

  it 'both hands have two pairs that add to the same value, win goes to highest pair', ->
    hands = {
      '6S 6H 3S 3H AS',
      '7H 7S 2H 2S AC',
    }
    expected = {'7H 7S 2H 2S AC'}
    assert.are.same expected, Poker.bestHands hands

  it 'two pairs first ranked by largest pair', ->
    hands = {
      '5C 2S 5S 4H 4C',
      '6S 2S 6H 7C 2C',
    }
    expected = {'6S 2S 6H 7C 2C'}
    assert.are.same expected, Poker.bestHands hands

  it 'three of a kind beats two pair', ->
    hands = {
      '2S 8H 2H 8D JH',
      '4S 5H 4C 8S 4H',
    }
    expected = {'4S 5H 4C 8S 4H'}
    assert.are.same expected, Poker.bestHands hands

  it 'both hands have three of a kind, tie goes to highest ranked triplet', ->
    hands = {
      '2S 2H 2C 8D JH',
      '4S AH AS 8C AD',
    }
    expected = {'4S AH AS 8C AD'}
    assert.are.same expected, Poker.bestHands hands

  it 'with multiple decks, two players can have same three of a kind, ties go to highest remaining cards', ->
    hands = {
      '5S AH AS 7C AD',
      '4S AH AS 8C AD',
    }
    expected = {'4S AH AS 8C AD'}
    assert.are.same expected, Poker.bestHands hands

  it 'a straight beats three of a kind', ->
    hands = {
      '4S 5H 4C 8D 4H',
      '3S 4D 2S 6D 5C',
    }
    expected = {'3S 4D 2S 6D 5C'}
    assert.are.same expected, Poker.bestHands hands

  it 'aces can end a straight (10 J Q K A)', ->
    hands = {
      '4S 5H 4C 8D 4H',
      '10D JH QS KD AC',
    }
    expected = {'10D JH QS KD AC'}
    assert.are.same expected, Poker.bestHands hands

  it 'aces can start a straight (A 2 3 4 5)', ->
    hands = {
      '4S 5H 4C 8D 4H',
      '4D AH 3S 2D 5C',
    }
    expected = {'4D AH 3S 2D 5C'}
    assert.are.same expected, Poker.bestHands hands

  it 'aces cannot be in the middle of a straight (Q K A 2 3)', ->
    hands = {
      '2C 3D 7H 5H 2S',
      'QS KH AC 2D 3S',
    }
    expected = {'2C 3D 7H 5H 2S'}
    assert.are.same expected, Poker.bestHands hands

  it 'both hands with a straight, tie goes to highest ranked card', ->
    hands = {
      '4S 6C 7S 8D 5H',
      '5S 7H 8S 9D 6H',
    }
    expected = {'5S 7H 8S 9D 6H'}
    assert.are.same expected, Poker.bestHands hands

  it 'even though an ace is usually high, a 5-high straight is the lowest-scoring straight', ->
    hands = {
      '2H 3C 4D 5D 6H',
      '4S AH 3S 2D 5H',
    }
    expected = {'2H 3C 4D 5D 6H'}
    assert.are.same expected, Poker.bestHands hands

  it 'flush beats a straight', ->
    hands = {
      '4C 6H 7D 8D 5H',
      '2S 4S 5S 6S 7S',
    }
    expected = {'2S 4S 5S 6S 7S'}
    assert.are.same expected, Poker.bestHands hands

  it 'both hands have a flush, tie goes to high card, down to the last one if necessary', ->
    hands = {
      '2H 7H 8H 9H 6H',
      '3S 5S 6S 7S 8S',
    }
    expected = {'2H 7H 8H 9H 6H'}
    assert.are.same expected, Poker.bestHands hands

  it 'full house beats a flush', ->
    hands = {
      '3H 6H 7H 8H 5H',
      '4S 5H 4C 5D 4H',
    }
    expected = {'4S 5H 4C 5D 4H'}
    assert.are.same expected, Poker.bestHands hands

  it 'both hands have a full house, tie goes to highest-ranked triplet', ->
    hands = {
      '4H 4S 4D 9S 9D',
      '5H 5S 5D 8S 8D',
    }
    expected = {'5H 5S 5D 8S 8D'}
    assert.are.same expected, Poker.bestHands hands

  it 'with multiple decks, both hands have a full house with the same triplet, tie goes to the pair', ->
    hands = {
      '5H 5S 5D 9S 9D',
      '5H 5S 5D 8S 8D',
    }
    expected = {'5H 5S 5D 9S 9D'}
    assert.are.same expected, Poker.bestHands hands

  it 'four of a kind beats a full house', ->
    hands = {
      '4S 5H 4D 5D 4H',
      '3S 3H 2S 3D 3C',
    }
    expected = {'3S 3H 2S 3D 3C'}
    assert.are.same expected, Poker.bestHands hands

  it 'both hands have four of a kind, tie goes to high quad', ->
    hands = {
      '2S 2H 2C 8D 2D',
      '4S 5H 5S 5D 5C',
    }
    expected = {'4S 5H 5S 5D 5C'}
    assert.are.same expected, Poker.bestHands hands

  it 'with multiple decks, both hands with identical four of a kind, tie determined by kicker', ->
    hands = {
      '3S 3H 2S 3D 3C',
      '3S 3H 4S 3D 3C',
    }
    expected = {'3S 3H 4S 3D 3C'}
    assert.are.same expected, Poker.bestHands hands

  it 'straight flush beats four of a kind', ->
    hands = {
      '4S 5H 5S 5D 5C',
      '7S 8S 9S 6S 10S',
    }
    expected = {'7S 8S 9S 6S 10S'}
    assert.are.same expected, Poker.bestHands hands

  it 'aces can end a straight flush (10 J Q K A)', ->
    hands = {
      'KC AH AS AD AC',
      '10C JC QC KC AC',
    }
    expected = {'10C JC QC KC AC'}
    assert.are.same expected, Poker.bestHands hands

  it 'aces can start a straight flush (A 2 3 4 5)', ->
    hands = {
      'KS AH AS AD AC',
      '4H AH 3H 2H 5H',
    }
    expected = {'4H AH 3H 2H 5H'}
    assert.are.same expected, Poker.bestHands hands

  it 'aces cannot be in the middle of a straight flush (Q K A 2 3)', ->
    hands = {
      '2C AC QC 10C KC',
      'QH KH AH 2H 3H',
    }
    expected = {'2C AC QC 10C KC'}
    assert.are.same expected, Poker.bestHands hands

  it 'both hands have a straight flush, tie goes to highest-ranked card', ->
    hands = {
      '4H 6H 7H 8H 5H',
      '5S 7S 8S 9S 6S',
    }
    expected = {'5S 7S 8S 9S 6S'}
    assert.are.same expected, Poker.bestHands hands

  it 'even though an ace is usually high, a 5-high straight flush is the lowest-scoring straight flush', ->
    hands = {
      '2H 3H 4H 5H 6H',
      '4D AD 3D 2D 5D',
    }
    expected = {'2H 3H 4H 5H 6H'}
    assert.are.same expected, Poker.bestHands hands
