dbg = require("debugger")
vlq = require("variable-length-quantity")

dbg.call(vlq.decode({0x40}))
