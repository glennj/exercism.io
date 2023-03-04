// Package twofer comment
package twofer

// ShareWith should have a comment documenting it.
func ShareWith(name string) string {
    who := "you"
	if len(name) > 0 {
        who = name
    }
	return "One for " + who + ", one for me."
}
