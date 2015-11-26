#=========================================================
# Makefile: run make exec or make all on Mac OS or linux
# When Fides isn't installed
# run make jobproto.elf spmproto.elf for sancus
#=========================================================

ifeq ($(uname_S),Darwin)
	SPECIAL_CONFS = 
endif

ifeq ($(uname_S),Linux)
	SPECIAL_CONFS = linux
endif

TARGET = jobproto.elf
TARGETSPM= spmproto.elf
TARGET_NO_HMAC = jobproto-no-hmac.elf
TARGET_NO_HMAC_SPM = spmproto-no-hmac.elf


FIDESSPM = Secure/cesk.spm
LIBS		= -lspm_loader
LIB_DIRS	= $(patsubst %,-L%,$(subst :, ,$(PCBAC_RESOURCES)))
SUBDIRS = Secure

# output and objects
FIDES_EXEC = fides_exec
MAC_EXEC = exec
NON_FIDES_OBJECTS = main.o Secure/cesk.o 
OBJECTS = main.o

CC = sancus-cc
LD = sancus-ld
CFLAGS = --verbose -DJOB -g 
LDFLAGS = --rom-size 48K --ram-size 10K --verbose --standalone
KEY = deadbeefcafebabedeadbeefcafebabe

FIDESFLAGS = -fPIC -fno-stack-protector -fno-builtin $(SPECIAL_CONFS) -fno-common -O2 -Wno-attributes

all: $(MAC_EXEC)

one: CC=gcc 
one: CFLAGS= -c -std=gnu99 -DFIDES  $(FIDESFLAGS)
one : $(OBJECTS)

output:
	$(LD) $(OBJECTS) $(FIDESSPM) -o $(FIDES_EXEC) $(LIB_DIRS) $(LIBS) 

$(TARGET_NO_HMAC): $(NON_FIDES_OBJECTS)
	$(LD) $(LDFLAGS) -o $@ $^

$(TARGET_NO_HMAC_SPM): CFLAGS = --verbose -DSANCUS_SPM -g -DSPM
$(TARGET_NO_HMAC_SPM): $(NON_FIDES_OBJECTS)
	$(LD) $(LDFLAGS) -o $@ $^

$(TARGETSPM): $(TARGET_NO_HMAC_SPM)
	sancus-hmac --verbose --key $(KEY) -o $@ $^

$(TARGET): $(TARGET_NO_HMAC) 
	sancus-hmac --verbose --key $(KEY) -o $@ $^

$(MAC_EXEC): CC=gcc
$(MAC_EXEC): CFLAGS=-g -std=gnu99 -DDEBUG  $(FIDESFLAGS)
$(MAC_EXEC): $(NON_FIDES_OBJECTS)
	gcc -o $(MAC_EXEC) $(NON_FIDES_OBJECTS)


clean:
	rm -f $(TARGET_NO_HMAC) $(TARGET) $(TARGETSPM) $(OBJECTS) $(NON_FIDES_OBJECTS)\
		$(TARGET_NO_HMAC_SPM) $(MAC_EXEC) sancus_sim.vcd
	@rm -r exec.dSYM

# only works on my setup...
download: all
	../../../../tools/bin/openmsp430-loader.tcl -device /dev/ttyUSB0 -baudrate 115200 ${TARGET}

debug:
	msp430-gdb -x debug.gdb

zip:
	zip sancus.zip *.c Makefile Readme *.h
		
