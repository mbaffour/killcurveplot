@echo off
rem ====================================================================
rem  Kill Curve Studio - double-click to run.
rem
rem  Creates a private virtual environment beside this file on first run,
rem  installs what it needs, then opens the app in your browser.
rem  Nothing is installed system-wide and nothing leaves your machine.
rem
rem  Finds Python in this order (first hit wins):
rem    1. KC_PYTHON env var (point it at python.exe to force a choice)
rem    2. the venv created by a previous run
rem    3. the Windows launcher  (py -3)
rem    4. python on PATH
rem    5. common per-user / conda / Program Files installs
rem ====================================================================
setlocal EnableDelayedExpansion
cd /d "%~dp0"

set "VENV=%~dp0.venv"
set "VPY=%VENV%\Scripts\python.exe"
set "PY="

rem --- 1. explicit override -------------------------------------------------
if defined KC_PYTHON if exist "%KC_PYTHON%" set "PY=%KC_PYTHON%"

rem --- 2. existing venv -----------------------------------------------------
if not defined PY if exist "%VPY%" (
  "%VPY%" -c "import flask,plotly,pandas" >nul 2>nul
  if not errorlevel 1 (
    echo Starting Kill Curve Studio...
    "%VPY%" -m app
    if errorlevel 1 goto :failed
    goto :eof
  )
)

rem --- 3. the py launcher ---------------------------------------------------
if not defined PY (
  py -3 -c "import sys;sys.exit(0 if sys.version_info>=(3,9) else 1)" >nul 2>nul
  if not errorlevel 1 for /f "delims=" %%P in ('py -3 -c "import sys;print(sys.executable)" 2^>nul') do set "PY=%%P"
)

rem --- 4. python on PATH ----------------------------------------------------
if not defined PY (
  python -c "import sys;sys.exit(0 if sys.version_info>=(3,9) else 1)" >nul 2>nul
  if not errorlevel 1 for /f "delims=" %%P in ('python -c "import sys;print(sys.executable)" 2^>nul') do set "PY=%%P"
)

rem --- 5. common install locations ------------------------------------------
if not defined PY (
  for %%C in (
    "%LOCALAPPDATA%\Programs\Python\Python313\python.exe"
    "%LOCALAPPDATA%\Programs\Python\Python312\python.exe"
    "%LOCALAPPDATA%\Programs\Python\Python311\python.exe"
    "%LOCALAPPDATA%\Programs\Python\Python310\python.exe"
    "%USERPROFILE%\Miniconda3\python.exe"
    "%USERPROFILE%\Anaconda3\python.exe"
    "%USERPROFILE%\miniforge3\python.exe"
    "%ProgramData%\Miniconda3\python.exe"
    "%ProgramData%\Anaconda3\python.exe"
    "C:\Python313\python.exe" "C:\Python312\python.exe" "C:\Python311\python.exe"
  ) do ( if not defined PY if exist "%%~C" set "PY=%%~C" )
)

if not defined PY (
  echo.
  echo   Python was not found.
  echo.
  echo   Install Python 3.9 or newer from https://www.python.org/downloads/
  echo   On the FIRST installer screen, tick "Add python.exe to PATH".
  echo.
  echo   Already have it somewhere unusual? Set KC_PYTHON to your python.exe
  echo   and run this file again.
  echo.
  pause
  exit /b 1
)

echo Using Python: %PY%

rem --- create the venv on first run ----------------------------------------
if not exist "%VPY%" (
  echo.
  echo First run - setting up a private environment. This takes a few minutes.
  echo.
  "%PY%" -m venv "%VENV%"
  if errorlevel 1 (
    echo.
    echo   Could not create the virtual environment.
    echo   If Python came from the Microsoft Store, install it from python.org instead.
    pause
    exit /b 1
  )
)

rem --- install / verify dependencies ---------------------------------------
"%VPY%" -c "import flask,plotly,pandas,numpy,scipy" >nul 2>nul
if errorlevel 1 (
  echo Installing packages ^(first run only^)...
  "%VPY%" -m pip install --upgrade pip --quiet
  "%VPY%" -m pip install --quiet -r requirements.txt
  if errorlevel 1 (
    echo.
    echo   Package installation failed. Check your internet connection and retry.
    echo   If you are behind a proxy, set HTTPS_PROXY before running this file.
    pause
    exit /b 1
  )
)

rem --- optional: the static-image engine needs a browser --------------------
rem  PNG/JPEG/SVG always work without this (they render in your own browser);
rem  it is only needed for PDF, TIFF, PowerPoint and GIF.
"%VPY%" -c "import kaleido" >nul 2>nul
if not errorlevel 1 (
  "%VPY%" -c "import plotly.io as pio,sys;from plotly.io import kaleido as k;sys.exit(0)" >nul 2>nul
)

echo.
echo Starting Kill Curve Studio - your browser will open shortly.
echo Close this window to stop the app.
echo.
"%VPY%" -m app
if errorlevel 1 goto :failed
goto :eof

:failed
echo.
echo   The app stopped with an error - see the messages above.
pause
exit /b 1
