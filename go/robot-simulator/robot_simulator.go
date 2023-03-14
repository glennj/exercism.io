package robot

// See defs.go for other definitions

// Step 1
const (
	N = 0
	W = 1
	S = 2
	E = 3
)

func Right() {
	Step1Robot.Dir = ((Step1Robot.Dir - 1) + 4) % 4
}

func Left() {
	Step1Robot.Dir = ((Step1Robot.Dir + 1) + 4) % 4
}

func Advance() {
	switch Step1Robot.Dir {
		case N: Step1Robot.Y++
		case E: Step1Robot.X++
		case S: Step1Robot.Y--
		case W: Step1Robot.X--
	}
}

func (d Dir) String() string {
	switch d {
		case E: return "East"
		case N: return "North"
		case W: return "West"
		case S: return "South"
		default: panic("why am I pointing this direction?")
	}
}

// Step 2
// Define Action type here.
type Action struct{}

func StartRobot(command chan Command, action chan Action) {
	panic("Please implement the StartRobot function")
}

func Room(extent Rect, robot Step2Robot, action chan Action, report chan Step2Robot) {
	panic("Please implement the Room function")
}

// Step 3
// Define Action3 type here.
type Action3 struct{}

func StartRobot3(name, script string, action chan Action3, log chan string) {
	panic("Please implement the StartRobot3 function")
}

func Room3(extent Rect, robots []Step3Robot, action chan Action3, rep chan []Step3Robot, log chan string) {
	panic("Please implement the Room3 function")
}
