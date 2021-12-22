    if { $argc != 2 } {
        puts "The add.tcl script requires two numbers to be inputed."
        puts "For example, tclsh add.tcl 2 5".
        puts "Please try again."
    } else {
	puts "Result:"
        puts [expr [lindex $argv 0] + [lindex $argv 1]]
        }
