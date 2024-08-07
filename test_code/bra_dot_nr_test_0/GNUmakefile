# These directories specify where source code files are located.
# Edit these variables if more directories are needed.  
# Separate each entry by spaces.


PREFIX:=flare-unknown-elf-

SHARED_SRC_DIRS:=src \

CXX_DIRS:=$(SHARED_SRC_DIRS)
C_DIRS:=$(SHARED_SRC_DIRS)
S_DIRS:=$(SHARED_SRC_DIRS)
# End of source directories


# Whether or not to do debugging stuff
#DEBUG:=yeah do debug

DEBUG_OPTIMIZATION_LEVEL:=-Og
REGULAR_OPTIMIZATION_LEVEL:=-O2


ALWAYS_DEBUG_SUFFIX:=_debug
ifdef DEBUG
	DEBUG_SUFFIX:=$(ALWAYS_DEBUG_SUFFIX)
endif

# This is the name of the output file.  Change this if needed!
PROJ:=$(shell basename $(CURDIR))$(DEBUG_SUFFIX).elf

# This is used for do_asmouts
#VERBOSE_ASM_FLAG:=-fverbose-asm

# Compilers and initial compiler flags
CXX:=$(PREFIX)g++
CXX_FLAGS:=$(CXX_FLAGS) -std=c++20 -fcoroutines -fmodules-ts -Wall

ifdef DEBUG
	CXX_FLAGS:=$(CXX_FLAGS) -DDEBUG=1
endif
CC:=$(PREFIX)gcc
C_FLAGS:=$(C_FLAGS) -std=c11 -Wall

AS:=$(PREFIX)as
S_FLAGS:=$(S_FLAGS) #-mlinkrelax

OBJDUMP:=$(PREFIX)objdump
OBJCOPY:=$(PREFIX)objcopy


#LD:=$(CXX)
LD:=$(PREFIX)ld.bfd
#LD:=$(PREFIX)ld

# Initial linker flags
#LD_FLAGS:=$(LD_FLAGS) -Wl,--relax #-lm
LD_FLAGS:=$(LD_FLAGS) #--relax



ifdef DEBUG
	EXTRA_DEBUG_FLAGS:=-g
	DEBUG_FLAGS:=-gdwarf-3 $(EXTRA_DEBUG_FLAGS)
	EXTRA_LD_FLAGS:=$(DEBUG_FLAGS)
	OPTIMIZATION_LEVEL:=$(DEBUG_OPTIMIZATION_LEVEL)
else
	OPTIMIZATION_LEVEL:=$(REGULAR_OPTIMIZATION_LEVEL)
endif




FINAL_BASE_FLAGS:=$(OPTIMIZATION_LEVEL) \
	$(EXTRA_BASE_FLAGS) $(EXTRA_DEBUG_FLAGS)

# Final compiler and linker flags
CXX_FLAGS:=$(CXX_FLAGS) $(FINAL_BASE_FLAGS)
C_FLAGS:=$(C_FLAGS) $(FINAL_BASE_FLAGS)
LD_FLAGS:=$(LD_FLAGS) $(EXTRA_LD_FLAGS) $(COMMON_LD_FLAGS)




# Generated directories
OBJDIR:=objs$(DEBUG_SUFFIX)
ASMOUTDIR:=asmouts$(DEBUG_SUFFIX)
DEPDIR:=deps$(DEBUG_SUFFIX)
PREPROCDIR:=preprocs$(DEBUG_SUFFIX)




CXX_SOURCES := $(foreach DIR,$(CXX_DIRS),$(wildcard $(DIR)/*.cpp))
CXX_OFILES := $(CXX_SOURCES:%.cpp=$(OBJDIR)/%.o)
CXX_PFILES := $(CXX_SOURCES:%.cpp=$(DEPDIR)/%.P)

# Assembly source code generated by gcc/g++
CXX_ASMOUTS := $(CXX_SOURCES:%.cpp=$(ASMOUTDIR)/%.s)


C_SOURCES := $(foreach DIR,$(C_DIRS),$(wildcard $(DIR)/*.c))
C_OFILES := $(C_SOURCES:%.c=$(OBJDIR)/%.o)
C_PFILES := $(C_SOURCES:%.c=$(DEPDIR)/%.P)

# Assembly source code generated by gcc/g++
C_ASMOUTS := $(C_SOURCES:%.c=$(ASMOUTDIR)/%.s)


S_SOURCES := $(foreach DIR,$(S_DIRS),$(wildcard $(DIR)/*.s))
S_OFILES := $(S_SOURCES:%.s=$(OBJDIR)/%.o)
S_PFILES := $(S_SOURCES:%.s=$(DEPDIR)/%.P)


# Compiler-generated files
# OFILES are object code files (extension .o)
OFILES:=$(CXX_OFILES) $(C_OFILES) $(S_OFILES)
# PFILES are used for automatic dependency generation
PFILES:=$(CXX_PFILES) $(C_PFILES) $(S_PFILES)
ASMOUTS:=$(CXX_ASMOUTS) $(C_ASMOUTS)

# Preprocessed output of C++ and/or C files
CXX_EFILES := $(CXX_SOURCES:%.cpp=$(PREPROCDIR)/%.E)
C_EFILES := $(C_SOURCES:%.c=$(PREPROCDIR)/%.E)
EFILES:=$(CXX_EFILES) $(C_EFILES)


.PHONY : all
all : all_pre $(OFILES)
	$(LD) $(OFILES) -o $(PROJ) $(LD_FLAGS)


# all_objs is ENTIRELY optional
.PHONY : all_objs
all_objs : all_pre $(OFILES)
	@#


.PHONY : do_asmouts
do_asmouts : all_pre all_pre_asmout $(ASMOUTS)
	@#


.PHONY : all_pre
all_pre :
	mkdir -p $(OBJDIR) $(DEPDIR)
	@for ofile in $(OFILES); \
	do \
		mkdir -p $$(dirname $$ofile); \
	done
	@for pfile in $(PFILES); \
	do \
		mkdir -p $$(dirname $$pfile); \
	done



.PHONY : all_pre_asmout
all_pre_asmout :
	mkdir -p $(ASMOUTDIR)
	@for asmout in $(ASMOUTS); \
	do \
		mkdir -p $$(dirname $$asmout); \
	done


# Here's where things get really messy. 
$(CXX_OFILES) : $(OBJDIR)/%.o : %.cpp
	@echo $@" was updated or has no object file.  (Re)Compiling...."
	$(CXX) $(CXX_FLAGS) -MMD -c $< -o $@
	@cp $(OBJDIR)/$*.d $(DEPDIR)/$*.P
	@rm -f $(OBJDIR)/$*.d

$(C_OFILES) : $(OBJDIR)/%.o : %.c
	@echo $@" was updated or has no object file.  (Re)Compiling...."
	$(CC) $(C_FLAGS) -MMD -c $< -o $@
	@cp $(OBJDIR)/$*.d $(DEPDIR)/$*.P
	@rm -f $(OBJDIR)/$*.d


$(S_OFILES) : $(OBJDIR)/%.o : %.s
	@echo $@" was updated or has no object file.  (Re)Assembling...."
	$(AS) $(S_FLAGS) -MD $(OBJDIR)/$*.d -c $< -o $@
	@cp $(OBJDIR)/$*.d $(DEPDIR)/$*.P
	@rm -f $(OBJDIR)/$*.d


# Here we have stuff for outputting assembly source code instead of an object file.
$(CXX_ASMOUTS) : $(ASMOUTDIR)/%.s : %.cpp
	$(CXX) $(CXX_FLAGS) -MMD -S $(VERBOSE_ASM_FLAG) $< -o $@
	@cp $(ASMOUTDIR)/$*.d $(DEPDIR)/$*.P
	@rm -f $(ASMOUTDIR)/$*.d

$(C_ASMOUTS) : $(ASMOUTDIR)/%.s : %.c
	$(CC) $(C_FLAGS) -MMD -S $(VERBOSE_ASM_FLAG) $< -o $@
	@cp $(ASMOUTDIR)/$*.d $(DEPDIR)/$*.P
	@rm -f $(ASMOUTDIR)/$*.d



-include $(PFILES)

#¯\(°_o)/¯

.PHONY : only_preprocess
only_preprocess : only_preprocess_pre $(EFILES)

.PHONY : only_preprocess_pre
only_preprocess_pre :
	mkdir -p $(DEPDIR) $(PREPROCDIR)
	@for efile in $(EFILES); \
	do \
		mkdir -p $$(dirname $$efile); \
	done
	@for pfile in $(PFILES); \
	do \
		mkdir -p $$(dirname $$pfile); \
	done


$(CXX_EFILES) : $(PREPROCDIR)/%.E : %.cpp
	$(CXX) $(CXX_FLAGS) -MMD -E $< -o $@
	@cp $(PREPROCDIR)/$*.d $(DEPDIR)/$*.P
	@rm -f $(PREPROCDIR)/$*.d

$(C_EFILES) : $(PREPROCDIR)/%.E : %.c
	$(CC) $(C_FLAGS) -MMD -E $< -o $@
	@cp $(PREPROCDIR)/$*.d $(DEPDIR)/$*.P
	@rm -f $(PREPROCDIR)/$*.d



.PHONY : clean
clean :
	rm -rfv $(OBJDIR) $(DEPDIR) $(ASMOUTDIR) $(PREPROCDIR) $(PROJ) tags *.taghl gmon.out


# Flags for make disassemble*
DISASSEMBLE_FLAGS:=$(DISASSEMBLE_BASE_FLAGS) -C -d
DISASSEMBLE_ALL_FLAGS:=$(DISASSEMBLE_BASE_FLAGS) -C -D

DISASSEMBLE_2_FLAGS:=$(DISASSEMBLE_BASE_FLAGS) -C -S -l -d
DISASSEMBLE_ALL_2_FLAGS:=$(DISASSEMBLE_BASE_FLAGS) -C -S -l -D

.PHONY : text-bin
text-bin: all
	$(OBJCOPY) $(PROJ) $(PROJ).bin --only-section=.text -O binary
	xxd $(PROJ).bin > $(PROJ).bin.txt

.PHONY : disassemble
disassemble :
	$(OBJDUMP) $(DISASSEMBLE_FLAGS) $(PROJ)

.PHONY : disassemble_all
disassemble_all :
	$(OBJDUMP) $(DISASSEMBLE_ALL_FLAGS) $(PROJ)


.PHONY : disassemble_2
disassemble_2 :
	$(OBJDUMP) $(DISASSEMBLE_2_FLAGS) $(PROJ)

.PHONY : disassemble_all_2
disassemble_all_2 :
	$(OBJDUMP) $(DISASSEMBLE_ALL_2_FLAGS) $(PROJ)
