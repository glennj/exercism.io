package blackjack

// ParseCard returns the integer value of a card following blackjack ruleset.
func ParseCard(card string) int {
	switch card {
	case "ace":
		return 11
	case "king", "queen", "jack", "ten":
		return 10
	case "nine":
		return 9
	case "eight":
		return 8
	case "seven":
		return 7
	case "six":
		return 6
	case "five":
		return 5
	case "four":
		return 4
	case "three":
		return 3
	case "two":
		return 2
	default:
		return 0
	}
}

// FirstTurn returns the decision for the first turn, given two cards of the
// player and one card of the dealer.
// Returns a 1-character string:
// - Stand (S)
// - Hit (H)
// - Split (P)
// - Automatically win (W)
func FirstTurn(card1, card2, dealerCard string) string {
	p := ParseCard(card1) + ParseCard(card2)
	d := ParseCard(dealerCard)

	switch {
    case p == 22:   // pair of aces
		return "P"
	case p == 21 && d < 10:
		return "W"
	case p == 21:
		return "S"
	case 17 <= p && p <= 20:
		return "S"
	case 12 <= p && p <= 16 && d >= 7:
		return "H"
	case 12 <= p && p <= 16:
		return "S"
	default:
		return "H"
	}
}
