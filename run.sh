#!/bin/bash
# Double-click me (or run ./this) to start the app. It opens in your web browser.
cd "$(dirname "$0")"
if command -v Rscript >/dev/null 2>&1; then RS=Rscript
elif [ -x /Library/Frameworks/R.framework/Resources/bin/Rscript ]; then RS=/Library/Frameworks/R.framework/Resources/bin/Rscript
else
  echo "R (Rscript) was not found. Install R from https://cran.r-project.org/ and try again."
  read -r -p "Press Enter to close..." _ ; exit 1
fi
echo "Starting — a browser window will open shortly."
echo "(First run installs R packages; this can take a few minutes.)"
"$RS" run_app.R
