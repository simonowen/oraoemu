@echo off

if "%1"=="clean" goto clean

opimpl.pl
pyz80.py --exportfile=oraoemu.sym oraoemu.asm
goto end

:clean
if exist oraoemu.dsk del oraoemu.dsk oraoemu.sym

:end
