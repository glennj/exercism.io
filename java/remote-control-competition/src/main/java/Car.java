abstract class Car {
    private static final int SPEED = 0;
    private int distanceDriven = 0;

    public void drive() {
        this.distanceDriven += SPEED;
    }

    public int getDistanceTravelled() {
        return this.distanceDriven;
    }
}
