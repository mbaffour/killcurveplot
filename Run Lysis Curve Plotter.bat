@echo off
setlocal enabledelayedexpansion
cd /d "%~dp0"
set "RS="
where Rscript >nul 2>nul && set "RS=Rscript"
if not defined RS (
  for /f "delims=" %%R in ('dir /b /o-n "C:\Program Files\R" 2^>nul ^| findstr /b "R-"') do (
    if not defined RS if exist "C:\Program Files\R\%%R\bin\Rscript.exe" set "RS=C:\Program Files\R\%%R\bin\Rscript.exe"
  )
)
if not defined RS (
  echo Could not find R. Please install R from https://cran.r-project.org/ then try again.
  pause
  exit /b 1
)
echo Starting - a browser window will open shortly.
echo (First run installs R packages; this can take a few minutes.)
"%RS%" run_app.R
if errorlevel 1 (
  echo.
  echo The app stopped with an error - see the messages above.
  pause
)
