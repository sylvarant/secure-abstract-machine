#=========================================================
# Makefile: Fides only makefile
#=========================================================

CC   	= gcc
AS		= gcc
CFLAGS		= -fPIC -fno-stack-protector -fno-builtin -fno-jump-tables -fno-common -O2 $(COPTIONS)
PCBAC_SPM_NAME	= cesk
PCBAC_EMULATE	= #--emulate
PCBAC_DYNAMIC	= #--dynamic
PCBAC_VERBOSE	= #--verbose
PCBAC_HEAP_SIZE	= 8192
PCBAC_STACK_SIZE= 10048
PCBAC_LIB_DIR	= #-L <path>
PCBAC_LIBS	= -lspm_core
PCBAC_CC 	= pcbac-x86_64-fides-cc
PCBAC_LD	= pcbac-x86_64-fides-ld
ARFLAGS 	= -rcs
INCLUDE		= 
OBJECTS 	= cesk.spmo
GENOBJECTS	= $(subst .spmo,.bc,$(OBJECTS))			\
			$(subst .spmo,.spm.bc,$(OBJECTS))	\
			$(subst .spmo,.spm.s,$(OBJECTS))	\
			stripped.sym
SPM		= $(PCBAC_SPM_NAME).spm

.SILENT:
all: $(SPM)

$(SPM): $(OBJECTS)
	echo "		[LD] " $(PCBAC_LD) --spm-name $(PCBAC_SPM_NAME) $(PCBAC_VERBOSE) $(PCBAC_DYNAMIC) $(PCBAC_EMULATE) -o $(SPM) $(OBJECTS)
	$(PCBAC_LD) --spm-name $(PCBAC_SPM_NAME) --heap-size $(PCBAC_HEAP_SIZE) --stack-size $(PCBAC_STACK_SIZE) $(PCBAC_VERBOSE) $(PCBAC_DYNAMIC) $(PCBAC_EMULATE) $(PCBAC_LIB_DIRS) $(OBJECTS) $(PCBAC_LIBS) -o $(SPM) 

%.spmo : %.c
	echo "		[CC] " $(PCBAC_CC) $(PCBAC_VERBOSE) -i $< 
	$(PCBAC_CC) $(PCBAC_CFLAGS) -i $<

