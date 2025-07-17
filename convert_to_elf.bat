@echo off
setlocal enabledelayedexpansion

REM Parameters: inputFile outputDir baseName nasmPath
set "inputFile=%~1"
set "outputDir=%~2" 
set "baseName=%~3"
set "nasmPath=%~4"

REM Create temporary file name
set "tempFile=%outputDir%\temp_%baseName%.asm"
set "elfFile=%outputDir%\%baseName%.elf"

REM Read input file and process line by line
(
    for /f "usebackq delims=" %%a in ("%inputFile%") do (
        set "line=%%a"
        
        REM Replace [org 0x0100] with [bits 16] (preserving whitespace)
        set "line=!line:[org 0x0100]=[bits 16]!"
        
        REM Replace standalone org 0x0100 with bits 16 (preserving whitespace) 
        set "line=!line:org 0x0100=bits 16!"
        
        echo !line!
    )
) > "%tempFile%"

REM Assemble to ELF
"%nasmPath%" "%tempFile%" -o "%elfFile%" -f elf

REM Check if assembly was successful
if %errorlevel% equ 0 (
    echo ELF file created: %elfFile%
    REM Clean up temp file
    del "%tempFile%"
) else (
    echo Assembly failed!
    echo Temp file left at: %tempFile%
)

endlocal