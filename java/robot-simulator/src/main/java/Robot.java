public class Robot {
    private Orientation orientation;
    private GridPosition gridPosition;

    Robot(GridPosition gp, Orientation o) {
        orientation = o;
        gridPosition = gp;
    }

    public Orientation  getOrientation()  { return orientation; }
    public GridPosition getGridPosition() { return gridPosition; }

    public void simulate(String script) throws IllegalArgumentException {
        script.chars().forEach(instruction -> {
            switch ((char) instruction) {
                case 'A': advance();   break;
                case 'L': turnLeft();  break;
                case 'R': turnRight(); break;
                default:  throw new IllegalArgumentException("unknown instruction.");
            }
        });
    }

    public void advance() {
        int x = gridPosition.x;
        int y = gridPosition.y;

        switch (orientation) {
            case NORTH: y++; break;
            case EAST:  x++; break;
            case SOUTH: y--; break;
            case WEST:  x--; break;
        }
        gridPosition = new GridPosition(x, y);
    }

    public void turnRight() {
        switch (orientation) {
            case NORTH: orientation = Orientation.EAST;  break;
            case EAST:  orientation = Orientation.SOUTH; break;
            case SOUTH: orientation = Orientation.WEST;  break;
            case WEST:  orientation = Orientation.NORTH; break;
        }
    }

    public void turnLeft() {
        switch (orientation) {
            case NORTH: orientation = Orientation.WEST;  break;
            case EAST:  orientation = Orientation.NORTH; break;
            case SOUTH: orientation = Orientation.EAST;  break;
            case WEST:  orientation = Orientation.SOUTH; break;
        }
    }
}
