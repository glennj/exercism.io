class ProductionRemoteControlCar implements RemoteControlCar, Comparable<ProductionRemoteControlCar> {
    private static final int SPEED = 10;
    private int distanceDriven = 0;
    private int victories = 0;

    public void drive() {
        distanceDriven += SPEED;
    }

    public int getDistanceTravelled() {
        return distanceDriven;
    }

    public int compareTo(ProductionRemoteControlCar other) {
        return Integer.compare(
                this.getNumberOfVictories(),
                other.getNumberOfVictories()
        );
    }

    public int getNumberOfVictories() {
        return victories;
    }

    public void setNumberOfVictories(int numberOfVictories) {
        victories = numberOfVictories;
    }
}
