#=========================================================
# Makefile: run exec on Mac OS or linux
#=========================================================

TARGET = jobproto.elf
TARGETSPM= spmproto.elf
TARGET_NO_HMAC = jobproto-no-hmac.elf
TARGET_NO_HMAC_SPM = spmproto-no-hmac.elf
MAC_EXEC = exec
OBJECTS = main.o attacker.o environment.o securecek.o 

CC = sancus-cc
LD = sancus-ld
CFLAGS = --verbose -DJOB -g 
LDFLAGS = --rom-size 48K --ram-size 10K --verbose --standalone
KEY = deadbeefcafebabedeadbeefcafebabe

all: $(TARGET) $(TARGETSPM) 

$(TARGET_NO_HMAC): $(OBJECTS)
	$(LD) $(LDFLAGS) -o $@ $^

$(TARGET_NO_HMAC_SPM): CFLAGS = --verbose -DJOB -g -DSPM
$(TARGET_NO_HMAC_SPM): $(OBJECTS)
	$(LD) $(LDFLAGS) -o $@ $^

$(TARGETSPM): $(TARGET_NO_HMAC_SPM)
	sancus-hmac --verbose --key $(KEY) -o $@ $^

$(TARGET): $(TARGET_NO_HMAC) 
	sancus-hmac --verbose --key $(KEY) -o $@ $^


$(MAC_EXEC): CC=gcc
$(MAC_EXEC): CFLAGS=-g -std=gnu99 -DDEBUG 
$(MAC_EXEC): $(OBJECTS)
	gcc -o $(MAC_EXEC) $(OBJECTS)


clean:
	rm -f $(TARGET_NO_HMAC) $(TARGET) $(TARGETSPM) $(OBJECTS) $(TARGET_NO_HMAC_SPM) $(MAC_EXEC) sancus_sim.vcd
	rm -r exec.dSYM

download: all
	../../../../tools/bin/openmsp430-loader.tcl -device /dev/ttyUSB0 -baudrate 115200 ${TARGET}

debug:
	msp430-gdb -x debug.gdb

zip:
	zip sancus.zip *.c Makefile Readme *.h
		
