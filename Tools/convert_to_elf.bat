@echo off
setlocal enabledelayedexpansion

REM Parameters: inputFile outputDir baseName nasmPath
set "inputFile=%~1"
set "outputDir=%~2" 
set "baseName=%~3"
set "nasmPath=%~4"

REM Create file names
set "backupFile=%outputDir%\%baseName%.bak"
set "objFile=%outputDir%\%baseName%.o"
set "elfFile=%outputDir%\%baseName%.elf"

REM Create a backup of the original file
copy "%inputFile%" "%backupFile%" > nul

REM Process the file in-place
echo Modifying file: %inputFile%
set "modified=false"

REM Do true inline replacement
sed -i "s/\[org 0x0100\]/[bits 16]/g" "%inputFile%"
sed -i "s/org 0x0100/bits 16/g" "%inputFile%"

REM Assemble to ELF
echo Assembling to ELF format...
"%nasmPath%" -f elf32 -g3 -F dwarf "%inputFile%" -o "%objFile%"
i386-elf-ld -Ttext=0x0100 "%objFile%" -o "%elfFile%"

REM Check if assembly was successful
if %errorlevel% equ 0 (
    echo ELF file created: %elfFile%
    REM Clean up temp files
    del "%objFile%"
) else (
    echo Assembly failed!
    echo Object file left at: %objFile%
)

REM Restore original file
echo Restoring original file...
copy /y "%backupFile%" "%inputFile%" > nul
del "%backupFile%"

echo Process complete.
endlocal