# Tcl package index file, version 1.1
# This file is generated by the "pkg_mkIndex" command
# and sourced either when an application starts up or
# by a "package unknown" script.  It invokes the
# "package ifneeded" command to set up package-related
# information so that packages will be loaded automatically
# in response to "package require" commands.  When this
# script is sourced, the variable $dir must contain the
# full path name of this file's directory.

package ifneeded structures::sequenceable 0.1 [list source [file join $dir sequenceable.tcl]]
package ifneeded structures::list 0.1 [list source [file join $dir list.tcl]]
package ifneeded structures::set 0.1 [list source [file join $dir set.tcl]]
package ifneeded structures::stack 0.1 [list source [file join $dir stack.tcl]]
package ifneeded structures::queue 0.1 [list source [file join $dir queue.tcl]]
package ifneeded structures::deque 0.1 [list source [file join $dir deque.tcl]]
