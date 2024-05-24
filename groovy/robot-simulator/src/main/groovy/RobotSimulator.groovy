class RobotSimulator {
    private static Bearings = [north: 90, east: 0, south: 270, west: 180]
    private static Directions = Bearings.collectEntries { key, value -> [value, key] }

    int x, y
    String direction
    private int bearing

    RobotSimulator(int pos_x, int pos_y, String direction) {
        this.x = pos_x
        this.y = pos_y
        this.direction = direction
        this.bearing = Bearings[direction]
    }

    def move(String commands) {
        for (cmd in commands) {
            switch (cmd) {
                case "L": turn(+1); break
                case "R": turn(-1); break
                case "A": advance()
            }
        }
        this
    }

    private def turn(int direction) {
        this.bearing = Math.floorMod(this.bearing + direction * 90, 360)
        this.direction = Directions[this.bearing]
    }

    private advance() {
        this.x += Math.cos(Math.toRadians(this.bearing))
        this.y += Math.sin(Math.toRadians(this.bearing))
    }
}
