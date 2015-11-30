#! /bin/bash

make -C abstract-machine;
./abstract-machine/exec;
cd correspondence && ./test.sh

