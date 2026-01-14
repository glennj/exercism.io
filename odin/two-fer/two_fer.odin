package two_fer

import "core:fmt"
import "core:strings"

two_fer :: proc(name: string = "you") -> string {
    // take 1
	//return fmt.tprintf("One for %s, one for me.", name)

    // take 2
    parts := [3]string {"One for ", name, ", one for me."}
    str, _ := strings.concatenate(parts[:])
    defer delete(str)
    return str

    // take 3
    /* This approach produces baffling failure messages:
        expected two_fer() to be One for you, one for me., got One for you, one for me.
       What's the difference?
    */
    /*
    b, _ := strings.builder_make(8 + len(name) + 13)
    defer strings.builder_destroy(&b)
    return fmt.sbprint(&b, "One for ", name, ", one for me.", sep="")
    */
}
