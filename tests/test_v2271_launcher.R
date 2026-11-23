# ============================================================================
# v2.27.1: the Mac app "Lysis Curve Plotter.app" and the run_app.R options it uses (KC_PORT, KC_AUTO_STOP).
# Run from the repository root:  Rscript tests/test_v2271_launcher.R
# ============================================================================
if (!file.exists("run_app.R")) stop("Run from the repository root: Rscript tests/test_v2271_launcher.R")
pass <- 0L; fail <- 0L
check <- function(desc, cond) { if (isTRUE(cond)) { pass <<- pass + 1L; cat("PASS:", desc, "\n") } else { fail <<- fail + 1L; cat("FAIL:", desc, "\n") } }
app <- "Lysis Curve Plotter.app/Contents"
check("the bundle: Info.plist, the launcher and the icon", all(file.exists(file.path(app, c("Info.plist", "MacOS/launch", "Resources/AppIcon.icns")))))
pl <- paste(readLines(file.path(app, "Info.plist"), warn = FALSE), collapse = "\n")
check("Info.plist names the launcher and the icon, and is an application", grepl("<key>CFBundleExecutable</key><string>launch</string>", pl, fixed = TRUE) &&
        grepl("<key>CFBundleIconFile</key><string>AppIcon</string>", pl, fixed = TRUE) && grepl("<string>APPL</string>", pl, fixed = TRUE))
check("the launcher is executable", file.access(file.path(app, "MacOS/launch"), 1L) == 0L)
sh <- paste(readLines(file.path(app, "MacOS/launch"), warn = FALSE), collapse = "\n")
check("the launcher: one port (8765), reopens a running app, runs run_app.R with KC_PORT and KC_AUTO_STOP, logs to ~/Library/Logs",
      grepl("KC_PORT:-8765", sh, fixed = TRUE) && grepl('grep -q "Optical Density"', sh, fixed = TRUE) &&
        grepl('KC_PORT="$PORT" KC_AUTO_STOP=1 nohup "$RS" run_app.R', sh, fixed = TRUE) && grepl("Library/Logs/LysisCurvePlotter.log", sh, fixed = TRUE))
check("the launcher is valid bash", system2("bash", c("-n", shQuote(file.path(app, "MacOS/launch"))), stdout = FALSE, stderr = FALSE) == 0L)
ra <- paste(readLines("run_app.R", warn = FALSE), collapse = "\n")
check("run_app.R parses, reads KC_PORT and KC_AUTO_STOP, and still runs without them", !inherits(tryCatch(parse("run_app.R"), error = identity), "error") &&
        grepl('Sys.getenv("KC_PORT"', ra, fixed = TRUE) && grepl('Sys.getenv("KC_AUTO_STOP")', ra, fixed = TRUE) &&
        grepl("else shiny::runApp(app, launch.browser = TRUE)", ra, fixed = TRUE))
# the auto-stop wrapper, as run_app.R builds it, on a small app: a session ending schedules stopApp only when none is left
suppressPackageStartupMessages(library(shiny))
a <- shinyApp(fluidPage("x"), function(input, output, session) invisible(NULL))
expr <- parse(text = ra)
blk <- Filter(function(e) is.call(e) && identical(e[[1]], as.name("if")) && grepl("KC_AUTO_STOP", paste(deparse(e[[2]]), collapse = "")), as.list(expr))
check("run_app.R has one KC_AUTO_STOP block", length(blk) == 1L)
if (length(blk) == 1L) {
  ev <- new.env(); ev$app <- a; Sys.setenv(KC_AUTO_STOP = "1"); eval(blk[[1]][[3]], ev); Sys.unsetenv("KC_AUTO_STOP")
  ended <- list(); fake <- list(onSessionEnded = function(f) ended[[length(ended) + 1L]] <<- f)
  srv <- ev$app$serverFuncSource(); srv(NULL, NULL, fake); srv(NULL, NULL, fake)
  check("two tabs open: the count is 2", identical(ev$open_tabs, 2L))
  ended[[1]](); check("one tab closed: one left, the app keeps running", identical(ev$open_tabs, 1L))
  ended[[2]](); check("the last tab closed: none left (the stop is scheduled)", identical(ev$open_tabs, 0L))
}
cat("\n", pass, "passed,", fail, "failed\n")
if (fail > 0) quit(status = 1)
