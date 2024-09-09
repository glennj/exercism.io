class DndCharacter
  d6 = -> 1 + Math.floor(6 * Math.random())
  characteristics = ['strength', 'dexterity', 'constitution','intelligence' , 'wisdom', 'charisma']

  constructor: ->
    for c in characteristics
      this[c] = @ability()
    @hitpoints = 10 + @modifier(@constitution)

  modifier: (score) -> (score - 10) // 2

  ability: ->
    rolls = [d6(), d6(), d6(), d6()]
    rolls.reduce((sum, r) -> sum + r) - Math.min(rolls...)
    
module.exports = DndCharacter
