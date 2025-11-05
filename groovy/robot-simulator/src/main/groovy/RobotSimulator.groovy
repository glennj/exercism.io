class RobotSimulator {
    private static Bearings = [north: 90, east: 0, south: 270, west: 180]

    int x, y
    private int bearing

    RobotSimulator(int pos_x, int pos_y, String direction) {
        this.x = pos_x
        this.y = pos_y
        this.bearing = Bearings[direction]
    }

    // provides the `robot.direction` property
    def getDirection() { 
        Bearings.find {it.value == this.bearing}?.key
    }
    
    def move(String commands) {
        for (cmd in commands) {
            switch (cmd) {
                case "L": turn(+90); break
                case "R": turn(-90); break
                case "A": advance()
            }
        }
        this
    }

    private def turn(int rotation) {
        this.bearing = Math.floorMod(this.bearing + rotation, 360)
    }

    private advance() {
        def θ = Math.toRadians(this.bearing)
        this.x += Math.cos(θ)
        this.y += Math.sin(θ)
    }
}
