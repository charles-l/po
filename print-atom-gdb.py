#!/usr/bin/env python

import gdb

class ListPrintCommand(gdb.Command):
    """print atoms"""

    def __init__(self):
        super(ListPrintCommand, self).__init__("print-atom",
                gdb.COMMAND_DATA, gdb.COMPLETE_SYMBOL)

    def print_atom(self, a):
        if a == 0x0:
            return
        if str(a['type']) == 'ATOM':
            print("%s" % (a['sym'].string()), end='')
        elif str(a['type']) == 'CONS':
            print("(", end='')
            self.print_atom(a['car'])
            print(" ", end='')
            self.print_atom(a['cdr'])
            print(")", end='')
        elif str(a['type']) == 'LAMBDA':
            print("<lambda>", end='')
        elif str(a['type']) == 'FFI':
            print("<cffi>", end='')


    def invoke(self, argument, from_tty):
        args = gdb.string_to_argv(argument)
        if len(args) == 0:
            print("Argument required (list to iterate)")
            return

        expr = args[0]
        a = gdb.parse_and_eval(expr)
        self.print_atom(a)
        print("")


ListPrintCommand()
