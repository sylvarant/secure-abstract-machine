Secure CESK Machine Implementation
==============================================

Details
-------------------------------

This is a Fides/sancus enabled implementation of the CESK machine that results from the syntactic correspondence.
Given that malloc and free where not available during most of our tests on Sancus,
that part of the implementation still focusses on a simple static memory buffer which is only flushed at the end.
Using this for anything more than a prototype will require adding the necessary free's.

The PMA: the entrypoints are defined in:
* PMA.h

The secure cesk for MiniML consists of the files :
* Secure/cesk.h
* Secure/cesk.c

The attacker resides in : 
* main.c 


Compilation
---------------------------------

To compile a simple executable version of the machine for mac / linux run:
```
make exec
```
To compile a version of the machine that uses the fides Hypervisor for run-time protection run: 
```
make one; cd Secure; make; cd ..; make output
```
This of course requires a working fides toolchain install

To compile a version of the machine that runs on the sancus microprocessor run:
```
make spmproto.elf  
```

