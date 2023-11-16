public class ExperimentalRemoteControlCar implements RemoteControlCar {
    private static final int SPEED = 20;
    private int distanceDriven = 0;

    public void drive() {
        distanceDriven += SPEED;
    }

    public int getDistanceTravelled() {
        return distanceDriven;
    }
}
