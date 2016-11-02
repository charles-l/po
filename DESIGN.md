so most of the design was taken directly from the paper (http://scheme2006.cs.uchicago.edu/11-ghuloum.pdf)
* types are stored in the lower bits of 32 bit values
* heap pointers are 8 bit aligned so the type can be stored in the lower 3 bits
    * so you basically have to check the type in the lower 3 bits, then orl them to 0 to get the actual pointer
* strings
    * store the length in the first 4 bytes (i know, it's overkill)
    * strings currently only hold single byte values (TODO: add utf-8 support)
