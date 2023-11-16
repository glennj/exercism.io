public class ElonsToyCar {
    private int battery;
    private int distance;

    public static ElonsToyCar buy() {
        return new ElonsToyCar();
    }

    ElonsToyCar() {
        battery = 100;
        distance = 0;
    }

    public String distanceDisplay() {
        return "Driven %d meters".formatted(distance);
    }

    public String batteryDisplay() {
        if (battery == 0)
            return "Battery empty";

        return "Battery at %d%%".formatted(battery);
    }

    public void drive() {
        if (battery > 0) {
            distance += 20;
            battery -= 1;
        }
    }
}
