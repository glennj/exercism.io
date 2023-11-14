import java.util.HashMap;
import java.util.List;
import java.util.Map;

class DnDCharacter {

    private enum Characteristic {
        STRENGTH, DEXTERITY, CONSTITUTION, INTELLIGENCE, WISDOM, CHARISMA
    }

    private final Map<Characteristic, Integer> abilities = new HashMap<>();
    private int hitpoints;
    private Dice dice;

    DnDCharacter() {
        dice = new Dice(6, 4);
        for (Characteristic c : Characteristic.values()) {
            dice.roll();
            abilities.put(c, abilityScore());
        }
        hitpoints = 10 + modifier(getConstitution());
    }

    int getStrength()     { return abilities.get(Characteristic.STRENGTH); }
    int getDexterity()    { return abilities.get(Characteristic.DEXTERITY); }
    int getConstitution() { return abilities.get(Characteristic.CONSTITUTION); }
    int getIntelligence() { return abilities.get(Characteristic.INTELLIGENCE); }
    int getWisdom()       { return abilities.get(Characteristic.WISDOM); }
    int getCharisma()     { return abilities.get(Characteristic.CHARISMA); }
    int getHitpoints()    { return hitpoints; }

    int modifier(int input) {
        return (int) Math.floor((input - 10) / 2.0);
    }

    int ability(List<Integer> nums) {
        dice.setDice(nums);
        return abilityScore();
    }

    private int abilityScore() {
        return dice.sumOfLargest(3);
    }

    List<Integer> rollDice() {
        return dice.roll().getDice();
    }
}
