all:
	csi -s po.scm > scheme_entry.s
	cc -c scheme_entry.s
	cc -o scheme_test driver.c scheme_entry.o
