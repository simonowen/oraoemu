DISK=oraoemu.dsk
ROMS=bas13.rom crt13.rom

.PHONY: clean

$(DISK): oraoemu.asm opdefs.inc $(ROMS)
	pyz80.py --exportfile=oraoemu.sym oraoemu.asm

opdefs.inc: opdefs.pl opimpl.inc oraoemu.asm
	./opdefs.pl

clean:
	rm -f $(DISK) oraoemu.sym
