abstract class Fighter {
    boolean isVulnerable() {
        return false;
    }

    public String toString() {
        return "Fighter is a %s".formatted(this.getClass().getName());
    }

    abstract int damagePoints(Fighter fighter);
}

class Warrior extends Fighter {
    @Override
    int damagePoints(Fighter fighter) {
        return fighter.isVulnerable() ? 10 : 6;
    }
}

class Wizard extends Fighter {
    private boolean hasSpellPrepared = false;

    @Override
    boolean isVulnerable() {
        return !hasSpellPrepared;
    }

    @Override
    int damagePoints(Fighter fighter) {
        return hasSpellPrepared ? 12 : 3;
    }

    void prepareSpell() {
        hasSpellPrepared = true;
    }

}
