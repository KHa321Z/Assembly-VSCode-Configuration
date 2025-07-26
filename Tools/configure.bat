@echo off
SETLOCAL EnableDelayedExpansion

:: Check if a path argument is provided
IF "%~1"=="" (
    echo Please provide a path as an argument.
    exit /b 1
)

:: Set the path variable
SET WORKSPACE_PATH=%~1

:: Path to the DOSBox configuration file (modify as needed)
SET CONFIG_FILE_PATH=%WORKSPACE_PATH%\DOSBoxPortable\Data\settings\

:: Modify or create the configuration file with the new path
copy /Y "%CONFIG_FILE_PATH%\dummy.conf" "%CONFIG_FILE_PATH%\dosbox.conf" 
echo [autoexec] >> "%CONFIG_FILE_PATH%\dosbox.conf"
echo MOUNT C "%WORKSPACE_PATH%" >> "%CONFIG_FILE_PATH%\dosbox.conf"
echo C: >> "%CONFIG_FILE_PATH%\dosbox.conf"

ENDLOCAL