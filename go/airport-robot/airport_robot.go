package airportrobot

import "fmt"

type Greeter interface {
	LanguageName() string
	Greet(name string) string
	greetingTemplate() string
}

type German struct{}
type Italian struct{}
type Portuguese struct{}

func (r German) LanguageName() string     { return "German" }
func (r Italian) LanguageName() string    { return "Italian" }
func (r Portuguese) LanguageName() string { return "Portuguese" }

func (r German) greetingTemplate() string     { return "Hallo %s!" }
func (r Italian) greetingTemplate() string    { return "Ciao %s!" }
func (r Portuguese) greetingTemplate() string { return "Olá %s!" }

func (r German) Greet(name string) string     { return greet(r, name) }
func (r Italian) Greet(name string) string    { return greet(r, name) }
func (r Portuguese) Greet(name string) string { return greet(r, name) }

func greet(robot Greeter, name string) string {
	return fmt.Sprintf(robot.greetingTemplate(), name)
}

func SayHello(name string, robot Greeter) string {
	return fmt.Sprintf("I can speak %s: %s",
		robot.LanguageName(),
		robot.Greet(name))
}

/*
 * If I was not constrained by the tests, I would
 *
 * create a Robot struct to hold the language and template
 *
 * 		type Robot struct {
 * 			language string
 * 			template string
 * 		}
 *
 * implement the Greeter methods on Robot
 *
 * 		func (r Robot) LanguageName() string { return r.language }
 * 		func (r Robot) Greet(name string) string {
 * 			return fmt.Sprintf(r.template, name)
 * 		}
 *
 * create "constructor" methods for the various nationalities
 *
 * 		func NewGermanRobot() Robot { return Robot{"German", `Hallo %s!`} }
 *
 * ... same for Italian and Portuguese
 *
 * and SayHello is the same.
 *
 * Extend the exercise:
 *
 * since the robots are for customer service, they also need to give
 * directions. Implement a Director interface with PointToDestination
 * method that takes a destination string and returns a string. If the 
 * destination is "check in", point "up stairs"; if the destination
 * is "baggage pickup", point "down stairs".
 *
 * We can introduce a BaggageHandlerRobot. Baggage handler robots can
 * be Greeters: they greet their human supervisors but only in Cantonese.
 * They can also be Directers: given a luggage tag, they can point to
 * the truck that will take bags to a specific airplane.
 */
