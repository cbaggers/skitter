#|| No consing

We need to write data into a control rather than setting it.

this means we need to know how to do that, that info has to live somewhere.
This sounds like info that needs to live on the control. So we need to have
a functio nthat takes.. well.

either:
1. a thing of the same type and copy the contents e.g. (foo (v! 1 2 3))
2. the elements of the thing e.g. (foo 1 2 3)

hmm how does cffi do this?
They use method 1, this would mean we still need to allocate 1 vector per
loop (or to have one squirreled away).

What about a :from-foreign option, so we can tell it how to get a ptr to
into the foreign object we have. This is nice as we can use cffi and just add
extra functions for each input-source:

    (set-mouse-pos-from-foreign ptr)

hmm we still need to know how to set the data unless it is scalar or vector.

Also expand-from-foreign allocates. We would need an expand-from-foreign-into
method. and then arent we in the same place, we still dont know for sure about
length and such.

maybe we would make define-control also specify a foreign type.. again this
only gets us to the point where we need to pull the data, and cffi is still
gonna allocate. bums



||#
