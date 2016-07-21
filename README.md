cl-pack
=======

[www.cliki.net/cl-pack](http://www.cliki.net/cl-pack)

cl-pack supplies Perl/PHP/Ruby/Python compatible `pack()` and `unpack()` functions to allow easy use of (binary format) protocols and files with the above mentioned languages and C. cl-pack was released by Dan Ballard under the BSD-3-Clause license.

The purpose of cl-pack is to take native Lisp data like numbers, floats, and strings and encode it in a safe binary format in string that can then be written to a file or exchanged with another program while unpack can extract data from binary formats and protocols.

cl-pack has nearly full support for all features offered by Perl's pack. It boasts full support for most data types and formating rules from numbers and string to formating rules and grouping and templates.  cl-pack also supports endian safe floats as outlined by ruby. cl-pack is supports ASDF so as to make it easy to integrate into your existing system.  

## Instalation ##

cl-pack is available in [QuickLisp](https://www.quicklisp.org/beta/)

    CL-PACK> (ql:quickload "cl-pack")

## Example: ##

    CL-PACK> (pack "VgA*c*" #x41424344 161.99 " a string " 69 70 71)
    ;; => "DCBAC!ýq a string EFG"

    CL-PACK> (unpack "B8H2Ng" "ABCDEFC!ýq")
    ;; => "01000001"
    ;; => "42"
    ;; => 1128547654
    ;; => 161.99

**Documentation** is currently a bit sparse, but is contained in cl-pack.lisp. Additionally, a good overview of pack and unpack functions can be seen at [http://perldoc.perl.org/functions/pack.html](http://perldoc.perl.org/functions/pack.html).

Nearly every feature except a few esoteric ones are supported, check the documentation inside cl-pack.lisp if in doubt and if a feature you need isn't currently supported feel free to contact me and I'll see if I can add it.

**Note** I think the 0.2 release is about as feature complete as I feel I need to get at the moment so I'm pushing it out. If there are not major complaints, then in a bit it will be re-released as 1.0, and if there are complaints, well more code and another pre-1.0 release :smile:

**Cavets** cl-pack was developed on an x86 running Ubuntu with SBCL.  It should be endian safe where required and conform to host CPU endianness where required but I haven't been able to test on anything but x86.  I would hope that it would work with most Lisps out there.  Please feel free to get a hold of me if you have issues that need fixing.
