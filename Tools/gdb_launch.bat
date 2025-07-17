@echo off
gdb -ix gdb_init.txt main.elf ^
    -ex "set tdesc filename target.xml" ^
    -ex "file coden.elf" ^
    -ex "target remote localhost:1111"