include ../MkRules
R_HOME = ../../..

CFLAGS = -O3 -Wall -pedantic $(CSTD)
ifdef DEBUG
 CFLAGS += $(G_FLAG)
 LINKFLAGS =
 DLLFLAGS =
else
 LINKFLAGS = -s
 DLLFLAGS = -s
endif
CPPFLAGS = -I../../include $(arch_DEFS)

## Increase stack size to 64Mb
LARGE_STACK = -Wl,--stack=0x4000000

rcmdfn-CFLAGS = -DBINDIR='"$(BINDIR)"'

.PHONY : all makeMakedeps

all: makeMakedeps \
     $(R_HOME)/$(BINDIR)/Rgui.exe $(R_HOME)/$(BINDIR)/Rterm.exe \
     $(R_HOME)/$(BINDIR)/Rcmd.exe $(R_HOME)/$(BINDIR)/RSetReg.exe \
     $(R_HOME)/$(BINDIR)/open.exe \
     $(R_HOME)/bin/R.exe \
     $(R_HOME)/bin/Rscript.exe

$(R_HOME)/$(BINDIR)/Rgui.exe: Rgui.exe
	$(MKDIR) -p $(R_HOME)/$(BINDIR)
	$(CP) $^ $@

$(R_HOME)/$(BINDIR)/Rterm.exe: Rterm.exe
	$(MKDIR) -p $(R_HOME)/$(BINDIR)
	$(CP) $^ $@	

$(R_HOME)/$(BINDIR)/Rcmd.exe: Rcmd.exe
	$(MKDIR) -p $(R_HOME)/$(BINDIR)
	$(CP) $^ $@

$(R_HOME)/$(BINDIR)/RSetReg.exe: RSetReg.exe
	$(MKDIR) -p $(R_HOME)/$(BINDIR)
	$(CP) $^ $@

$(R_HOME)/$(BINDIR)/R.exe: R.exe
	$(MKDIR) -p $(R_HOME)/$(BINDIR)
	$(CP) $^ $@	

ifneq "$(R_ARCH)" ""

all: $(R_HOME)/$(BINDIR)/Rfe.exe \
     $(R_HOME)/$(BINDIR)/Rscript.exe \
     $(R_HOME)/$(BINDIR)/R.exe

$(R_HOME)/$(BINDIR)/Rfe.exe: Rfe.exe
	$(MKDIR) -p $(R_HOME)/$(BINDIR)
	$(CP) $^ $@

$(R_HOME)/bin/R.exe: Rfe.exe
	$(MKDIR) -p $(R_HOME)/bin
	$(CP) $^ $@	

$(R_HOME)/bin/Rscript.exe: Rfe.exe
	$(MKDIR) -p $(R_HOME)/bin
	$(CP) $^ $@	
endif

$(R_HOME)/$(BINDIR)/Rscript.exe: Rscript.exe
	$(MKDIR) -p $(R_HOME)/$(BINDIR)
	$(CP) $^ $@	

$(R_HOME)/$(BINDIR)/open.exe: open.exe
	$(MKDIR) -p $(R_HOME)/$(BINDIR)
	$(CP) $^ $@	

rgui-CPPFLAGS = -DWin32
rterm-CPPFLAGS = -DWin32

## -luser32 -lkernel32 -ladvapi32 -lshell32 are now standard
R-LIBS = -lole32 -luuid
Rcmd-LIBS = -lole32 -luuid
Rfe-LIBS = -lole32 -luuid
Rgui-LIBS = -L../../../$(IMPDIR) -lR -lRgraphapp
Rscript-LIBS = -L../../../$(IMPDIR) -lR -lRgraphapp -lshlwapi
Rterm-LIBS = -L../../../$(IMPDIR) -lR -lRgraphapp -lshlwapi

Rgui-LINKFLAGS = -mwindows $(LARGE_ADDR) $(LARGE_STACK)
Rterm-LINKFLAGS = $(LARGE_ADDR) $(LARGE_STACK)
Rscript-LINKFLAGS = $(LARGE_ADDR) $(LARGE_STACK)

Rgui.exe: graphappmain.o rgui.o rgico.o
Rterm.exe: graphappmain.o rterm.o rtico.o

ifeq "$(WIN)" "32"
rcico.o: Rcmd.exe.manifest
rgico.o: RGui32.manifest
rsico.o: Rscript.exe.manifest
rtico.o: Rterm32.manifest
else
RESFLAGS = -DWN64
rcico.o: Rcmd64.exe.manifest
rtico.o: Rterm64.manifest
rsico.o: Rscript64.exe.manifest
rgico.o: RGui64.manifest
endif

Rcmd.exe: rcmd.o ../rhome.o ../shext.o rcico.o rcmdfn.o Renviron.o

R.exe: R.o ../rhome.o ../shext.o rcico.o rcmdfn.o Renviron.o

Rfe.exe: Rfe.o ../rhome.o ../shext.o rcico.o rcmdfn.o Renviron.o

RSetReg.exe: RSetReg.o ../rhome.o rcico.o

Rscript.o: ../../unix/Rscript.c ../../include/Rversion.h
	$(CC) $(CFLAGS) -DBINDIR='"$(BINDIR)"' -I. -I../../include -DHAVE_CONFIG_H -c $< -o $@

Renviron.o: ../../main/Renviron.c
	$(CC) $(CFLAGS) -I. -I../../include -DRENVIRON_WIN32_STANDALONE -c $< -o $@

Rscript.exe: Rscript.o rsico.o 

## Used only in writing etc/.../Makeconf
Rpwd:
	$(MAKE) -C ../../include -f Makefile.win version
	$(MAKE) Rpwd.exe

Rpwd.exe: rpwd.o rcico.o

rcmd.o R.o rgui.o rterm.o RSetReg.o rgico.o rtico.o rcico.o rsico.c: ../../include/Rversion.h

open.o: open.c
open.exe: open.o

clean:
	$(RM) *.o *.obj *.exe *~ *.exp *.dll *.lib *.a *.d Makedeps

CSOURCES = $(filter-out R.c rcmd.c rpwd.c rtest.c, $(wildcard *.c))
DEPS = $(CSOURCES:.c=.d)

makeMakedeps: $(DEPS)
	@$(RM) Makedeps
ifneq ($(strip $(DEPS)),)
	@cat $(DEPS) >> Makedeps
endif

-include Makedeps
