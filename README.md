# skitter

skitter is a repl friendly event system for games.

It tries to achieve the *repl friendly* goal by only having strong refs to the parent by default. The references to the children are weak.

This means that lost event-nodes are cleaned up and so you have less 'weird behaviour' caused by leftover nodes you forgot about earlier.

This obviously puts extra load on the GC so it can easily be turned off.

Otherwise this seeks to have little in the way fancy features or extensibility.
