Here, we define a set of virtual objects which make up the abstraction
underlying the Editor.  We assume for the time being that the object of our
editing activities is ASCII text, although this need not be the case in
general.

    o   Buffer          A series of lines of text which constitute the
                        object we are editing, or which are intended to
                        appear on the display device (eg, The Status Line,
                        the Message Line, a Help Line, etc.).

    o   Window          A contiguous subset of the contents of a Buffer
                        which is intended to be displayed somewhere on the
                        display device.

    o   Display         A contiguous region of the display device's
                        topology, typically bound in some sense to a Window
                        into some Buffer.

    o   Screen          Collective term for the display device's
                        topology, and its distribution among the various
                        Displays bound to it.

    o   File            This is the source and sink of the information
                        residing in the Buffer being edited.
    