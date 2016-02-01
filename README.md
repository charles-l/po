# po
minimal lisp implementation

# What's implemented
- Basic parser

# To be implemented
- Bytecode?
- Jit?
- Profiler
- Standard libraries

# Bugs
- Not all memory is cleaned up right now

# Tools

To build:
- C compiler
- `make`

To develop:
- Debugger: GDB
- I wrote [this GDB](https://gist.github.com/charles-l/b4745f6ae14ddea4148d) script to print atoms in the debugger.
- `vim`
    - [Paredit](https://github.com/vim-scripts/paredit.vim)
    - [vim-surround](https://github.com/tpope/vim-surround)
