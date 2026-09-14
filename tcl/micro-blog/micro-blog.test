#!/usr/bin/env tclsh
# generated: 2026-07-18T18:47:22Z
package require tcltest
namespace import ::tcltest::*
source testHelpers.tcl

# Uncomment next line to view test durations.
#configure -verbose {body error usec}

############################################################
source "micro-blog.tcl"


test micro-blog-1 "English language short" -body {
    truncate "Hi"
} -returnCodes ok -match exact -result "Hi"

skip micro-blog-2
test micro-blog-2 "English language long" -body {
    truncate "Hello there"
} -returnCodes ok -match exact -result "Hello"

skip micro-blog-3
test micro-blog-3 "German language short (broth)" -body {
    truncate "brühe"
} -returnCodes ok -match exact -result "brühe"

skip micro-blog-4
test micro-blog-4 "German language long (bear carpet → beards)" -body {
    truncate "Bärteppich"
} -returnCodes ok -match exact -result "Bärte"

skip micro-blog-5
test micro-blog-5 "Bulgarian language short (good)" -body {
    truncate "Добър"
} -returnCodes ok -match exact -result "Добър"

skip micro-blog-6
test micro-blog-6 "Greek language short (health)" -body {
    truncate "υγειά"
} -returnCodes ok -match exact -result "υγειά"

skip micro-blog-7
test micro-blog-7 "Maths short" -body {
    truncate "a=πr²"
} -returnCodes ok -match exact -result "a=πr²"

skip micro-blog-8
test micro-blog-8 "Maths long" -body {
    truncate "∅⊊ℕ⊊ℤ⊊ℚ⊊ℝ⊊ℂ"
} -returnCodes ok -match exact -result "∅⊊ℕ⊊ℤ"

skip micro-blog-9
test micro-blog-9 "English and emoji short" -body {
    truncate "Fly 🛫"
} -returnCodes ok -match exact -result "Fly 🛫"

skip micro-blog-10
test micro-blog-10 "Emoji short" -body {
    truncate "💇"
} -returnCodes ok -match exact -result "💇"

skip micro-blog-11
test micro-blog-11 "Emoji long" -body {
    truncate "❄🌡🤧🤒🏥🕰😀"
} -returnCodes ok -match exact -result "❄🌡🤧🤒🏥"

skip micro-blog-12
test micro-blog-12 "Royal Flush?" -body {
    truncate "🃎🂸🃅🃋🃍🃁🃊"
} -returnCodes ok -match exact -result "🃎🂸🃅🃋🃍"


cleanupTests
