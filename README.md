# po
minimal lisp implementation

# goals
* Compile to ASM
* Good stdlib (like Ruby)
* Good C interoperability (maybe without even having to write bindings?!)
* Be high level without being stupid. The computer still exists, so allow (safe) low-level access to it. Abstractions of the computer/OS are for libraries.

(using [this paper](http://scheme2006.cs.uchicago.edu/11-ghuloum.pdf) as a reference)

# current status (requires `cc`)
`csi -s test.scm`
