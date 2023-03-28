package robot

// See defs.go for other definitions

// Step 1
const (
	N Dir = iota
	E
	S
	W
)

func Right() {
	switch Step1Robot.Dir {
	case N:
		Step1Robot.Dir = E
	case E:
		Step1Robot.Dir = S
	case S:
		Step1Robot.Dir = W
	case W:
		Step1Robot.Dir = N
	}
}

func Left() {
	switch Step1Robot.Dir {
	case N:
		Step1Robot.Dir = W
	case E:
		Step1Robot.Dir = N
	case S:
		Step1Robot.Dir = E
	case W:
		Step1Robot.Dir = S
	}
}

func Advance() {
	switch Step1Robot.Dir {
	case N:
		Step1Robot.Y++
	case E:
		Step1Robot.X++
	case S:
		Step1Robot.Y--
	case W:
		Step1Robot.X--
	}
}

func (d Dir) String() string {
	switch d {
	case N:
		return "North"
	case E:
		return "East"
	case S:
		return "South"
	default:
		return "West"
	}
}

// Step 2
// Define Action type here.
type Action func(Step2Robot) Step2Robot

func turnRight(r Step2Robot) Step2Robot {
	switch r.Dir {
	case N:
		return Step2Robot{E, r.Pos}
	case E:
		return Step2Robot{S, r.Pos}
	case S:
		return Step2Robot{W, r.Pos}
	default:
		return Step2Robot{N, r.Pos}
	}
}

func turnLeft(r Step2Robot) Step2Robot {
	switch r.Dir {
	case N:
		return Step2Robot{W, r.Pos}
	case E:
		return Step2Robot{N, r.Pos}
	case S:
		return Step2Robot{E, r.Pos}
	default:
		return Step2Robot{S, r.Pos}
	}
}

func advance(r Step2Robot) Step2Robot {
	switch r.Dir {
	case N:
		return Step2Robot{r.Dir, Pos{r.Easting, r.Northing + 1}}
	case E:
		return Step2Robot{r.Dir, Pos{r.Easting + 1, r.Northing}}
	case S:
		return Step2Robot{r.Dir, Pos{r.Easting, r.Northing - 1}}
	default:
		return Step2Robot{r.Dir, Pos{r.Easting - 1, r.Northing}}
	}
}

func StartRobot(command chan Command, action chan Action) {
	switch <-command {
	case 'R':
		action <- turnRight
	case 'L':
		action <- turnLeft
	case 'A':
		action <- advance
	}
}

func Room(extent Rect, robot Step2Robot, action chan Action, report chan Step2Robot) {
	act := <-action
	newRobot := act(robot)
	if !inRoom(extent, newRobot) {
		newRobot = robot
	}
	report <- newRobot
}

func inRoom(rect Rect, robot Step2Robot) bool {
	n, e := robot.Pos.Northing, robot.Pos.Easting
	return rect.Min.Easting <= e && e <= rect.Max.Easting &&
		rect.Min.Northing <= n && n <= rect.Max.Northing
}

// Step 3
// Define Action3 type here.
type Action3 int

func StartRobot3(name, script string, action chan Action3, log chan string) {
	panic("Please implement the StartRobot3 function")
}

func Room3(extent Rect, robots []Step3Robot, action chan Action3, rep chan []Step3Robot, log chan string) {
	panic("Please implement the Room3 function")
}
