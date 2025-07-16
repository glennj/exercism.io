oo::class create PlatformIO {
    variable io_stats

    constructor {} {
        set io_stats {reads 0 readBytes 0 writes 0 writeBytes 0}
    }

    forward open    my OpenChannel ::open
    forward socket  my OpenChannel ::socket
    
    method OpenChannel {cmd args} {
        set handle [$cmd {*}$args]
        ::chan push $handle [self object]
        return $handle
    }

    method stats {} { return $io_stats }
    
    #######################################
    # channel transform command handler API
    # https://www.tcl-lang.org/man/tcl9.0/TclCmd/transchan.html
    
    method initialize {args} {
        # initialize returns a list of supported API subcommands.
        return {initialize finalize read write}
    }

    # nothing to clean up here
    method finalize {handle} {}

    method read {handle buffer} {
        dict incr io_stats reads
        dict incr io_stats readBytes [string length $buffer]
        # no other transformation on the buffer
        return $buffer
    }

    method write {handle buffer} {
        dict incr io_stats writes
        dict incr io_stats writeBytes [string length $buffer]
        return $buffer
    }
}
