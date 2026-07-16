# Browser-driven smoke test: boots the real app in a subprocess and walks
# the first-run flow in headless Chrome — bootstrap admin creation, TOTP
# enrollment (computing a real code from the displayed setup key), recovery
# codes, and the logged-in app shell.
#
# Skipped unless the chromote package and a Chrome/Chromium binary are
# available (set CHROMOTE_CHROME to the executable if it isn't on PATH).

browser_smoke_available <- function() {
  if (!requireNamespace("chromote", quietly = TRUE)) {
    return(FALSE)
  }
  chrome <- Sys.getenv("CHROMOTE_CHROME", "")
  if (!nzchar(chrome)) {
    chrome <- tryCatch(chromote::find_chrome(), error = function(e) NULL)
  }
  !is.null(chrome) && nzchar(chrome) && file.exists(chrome)
}

test_that("first-run flow works in a real browser: bootstrap -> TOTP enrollment -> logged in", {
  if (!browser_smoke_available()) {
    testthat::skip("chromote/Chrome not available; skipping browser smoke test")
  }
  source_from_root("R/auth/totp.R")

  root <- rids_repo_root()
  temp_root <- withr::local_tempdir("rids_browser_smoke_")
  for (d in c("data", "uploads", "outputs", "logs")) {
    dir.create(file.path(temp_root, d), recursive = TRUE, showWarnings = FALSE)
  }

  port <- httpuv::randomPort()
  app_log <- file.path(temp_root, "app.log")

  app_proc <- processx::process$new(
    file.path(R.home("bin"), "Rscript"),
    c("-e", sprintf(
      "setwd('%s'); shiny::runApp('.', host = '127.0.0.1', port = %d, launch.browser = FALSE)",
      root, port
    )),
    env = c(
      Sys.getenv(),
      RIDS_STORAGE_MODE = "duckdb",
      RIDS_DB_DIR = file.path(temp_root, "data", "RIDS.duckdb"),
      RIDS_ICT_UPLOAD_DIR = file.path(temp_root, "uploads"),
      RIDS_EDGE_OUTPUT_DIR = file.path(temp_root, "outputs"),
      RIDS_APP_LOG_DIR = file.path(temp_root, "logs"),
      RIDS_CREDENTIAL_SECRET = "browser-smoke-secret-browser-smoke",
      RIDS_APP_STATUS = "dev",
      RIDS_CONFIG_PATH = "/nonexistent/deployment_config.R"
    ),
    stdout = app_log, stderr = "2>&1"
  )
  withr::defer(if (app_proc$is_alive()) app_proc$kill())

  # Wait for the app to answer HTTP
  app_url <- sprintf("http://127.0.0.1:%d", port)
  up <- FALSE
  for (i in 1:120) {
    up <- tryCatch({
      con <- url(app_url, open = "rb")
      close(con)
      TRUE
    }, error = function(e) FALSE, warning = function(w) FALSE)
    if (up) break
    if (!app_proc$is_alive()) break
    Sys.sleep(0.5)
  }
  if (!up) {
    testthat::fail(paste(
      "App did not start. Log tail:",
      paste(utils::tail(readLines(app_log, warn = FALSE), 15), collapse = "\n")
    ))
  }

  chrome_args <- tryCatch(chromote::get_chrome_args(), error = function(e) character())
  if (identical(Sys.info()[["effective_user"]], "root")) {
    chrome_args <- unique(c(chrome_args, "--no-sandbox", "--disable-dev-shm-usage"))
  }
  browser <- chromote::Chromote$new(browser = chromote::Chrome$new(args = chrome_args))
  b <- chromote::ChromoteSession$new(parent = browser)
  withr::defer({
    try(b$close(), silent = TRUE)
    try(browser$close(), silent = TRUE)
  })

  js <- function(expr) {
    b$Runtime$evaluate(expr, returnByValue = TRUE)$result$value
  }
  wait_for <- function(expr, timeout = 30, label = expr) {
    deadline <- Sys.time() + timeout
    while (Sys.time() < deadline) {
      ok <- tryCatch(isTRUE(js(expr)), error = function(e) FALSE)
      if (ok) return(invisible(TRUE))
      Sys.sleep(0.25)
    }
    testthat::fail(paste("Timed out waiting for:", label))
  }
  set_input <- function(id, value) {
    js(sprintf(
      "$('#%s').val(%s).trigger('input').trigger('change'); true",
      id, jsonlite::toJSON(value, auto_unbox = TRUE)
    ))
  }
  click <- function(id) {
    js(sprintf("$('#%s').click(); true", id))
  }

  b$Page$navigate(app_url)
  wait_for("window.Shiny !== undefined && $('#login-bootstrap_view:visible').length > 0",
           timeout = 60, label = "bootstrap view visible")

  # Create the initial admin account
  set_input("login-bootstrap_name", "Smoke Admin")
  set_input("login-bootstrap_username", "smoke.admin")
  set_input("login-bootstrap_password", "SmokePass123")
  set_input("login-bootstrap_confirm_password", "SmokePass123")
  click("login-bootstrap_admin")

  # Mandatory MFA enrollment: read the setup key shown in the UI
  wait_for("$('#login-mfa_enroll_view:visible').length > 0",
           label = "MFA enrollment view visible")
  wait_for("$('#login-enroll_secret_ui code').length > 0",
           label = "setup key rendered")
  secret <- js("$('#login-enroll_secret_ui code').first().text()")
  expect_match(secret, "^[A-Z2-7]+$")

  # Compute a real TOTP code from the displayed secret and activate
  set_input("login-enroll_code", totp_code_for_step(secret, totp_current_step()))
  click("login-confirm_enrollment")

  # Recovery codes modal appears once
  wait_for("$('.modal:visible pre').length > 0", label = "recovery codes modal")
  recovery_text <- js("$('.modal:visible pre').text()")
  expect_true(length(strsplit(trimws(recovery_text), "\n")[[1]]) == 8)
  js("$('.modal:visible button[data-dismiss], .modal:visible button[data-bs-dismiss]').click(); true")

  # Logged in: overlay hidden, user badge rendered with the admin's name
  wait_for("$('#login-overlay:visible').length === 0", label = "login overlay hidden")
  wait_for("$('#user_badge').text().indexOf('Smoke Admin') !== -1",
           label = "user badge shows the admin")
  badge <- js("$('#user_badge').text()")
  expect_match(badge, "Admin")

  # Narrow-screen help remains fully visible and keyboard-contained.
  b$Emulation$setDeviceMetricsOverride(
    width = 320L,
    height = 700L,
    deviceScaleFactor = 1,
    mobile = FALSE
  )
  click("sidebar-new_ict")
  wait_for("$('#app-step1-help-toggle:visible').length === 1",
           label = "Step 1 help control visible")
  expect_true(js(paste(
    "(function() {",
    "  var input = document.getElementById('app-step1-mff_split_enabled');",
    "  var text = input && input.closest('label').querySelector('span');",
    "  if (!input || !text) return false;",
    "  var gap = text.getBoundingClientRect().left - input.getBoundingClientRect().right;",
    "  return gap >= 7;",
    "})()"
  )))
  click("app-step1-help-toggle")
  wait_for(
    paste0(
      "$('#app-step1-help-panel:visible').length === 1 && ",
      "$('#app-step1-help-panel').attr('aria-hidden') === 'false'"
    ),
    label = "help dialog visible"
  )

  expect_true(js(paste(
    "(function() {",
    "  var panel = document.getElementById('app-step1-help-panel');",
    "  var rect = panel.getBoundingClientRect();",
    "  return rect.left >= 0 && rect.right <= window.innerWidth + 1;",
    "})()"
  )))
  expect_true(js(paste(
    "(function() {",
    "  var panel = document.getElementById('app-step1-help-panel');",
    "  return panel.contains(document.activeElement);",
    "})()"
  )))

  b$Input$dispatchKeyEvent(
    type = "keyDown", key = "Tab", code = "Tab", windowsVirtualKeyCode = 9L
  )
  b$Input$dispatchKeyEvent(
    type = "keyUp", key = "Tab", code = "Tab", windowsVirtualKeyCode = 9L
  )
  expect_true(js(
    "document.getElementById('app-step1-help-panel').contains(document.activeElement)"
  ))

  b$Input$dispatchKeyEvent(
    type = "keyDown", key = "Escape", code = "Escape", windowsVirtualKeyCode = 27L
  )
  b$Input$dispatchKeyEvent(
    type = "keyUp", key = "Escape", code = "Escape", windowsVirtualKeyCode = 27L
  )
  wait_for(
    paste0(
      "$('#app-step1-help-panel:visible').length === 0 && ",
      "$('#app-step1-help-toggle').attr('aria-expanded') === 'false'"
    ),
    label = "help dialog closed"
  )
  wait_for(
    "document.activeElement === document.getElementById('app-step1-help-toggle')",
    label = "focus returned to help control"
  )
  expect_false(js(paste(
    "(function() {",
    "  var help = document.getElementById('app-step1-help-toggle').getBoundingClientRect();",
    "  var next = document.getElementById('app-step1-next_step').getBoundingClientRect();",
    "  return !(help.right <= next.left || help.left >= next.right ||",
    "    help.bottom <= next.top || help.top >= next.bottom);",
    "})()"
  )))

  # Shared mobile filter grids stay inside the viewport.
  js("$('.main-sidebar [data-value=\"tab_library\"]').first().click(); true")
  wait_for("$('#app-library-search:visible').length === 1",
           label = "Study Library visible")
  expect_true(js(paste(
    "(function() {",
    "  var bar = document.querySelector('.library-filter-bar');",
    "  if (!bar) return false;",
    "  var right = bar.getBoundingClientRect().right + 1;",
    "  var controls = bar.querySelectorAll('.form-group, .rids-filter-action');",
    "  return document.documentElement.scrollWidth <= window.innerWidth &&",
    "    Array.from(controls).every(function(control) {",
    "      return control.getBoundingClientRect().right <= right;",
    "    });",
    "})()"
  )))

  # Wide admin data stays in a labelled, keyboard-focusable scroll region.
  js("$('.main-sidebar [data-value=\"tab_admin\"]').first().click(); true")
  wait_for("$('.rids-admin-users-table:visible').length === 1",
           label = "Admin users table visible")
  expect_true(js(paste(
    "(function() {",
    "  var region = document.querySelector('.rids-admin-users-table');",
    "  return document.documentElement.scrollWidth <= window.innerWidth &&",
    "    region.getAttribute('role') === 'region' &&",
    "    region.getAttribute('tabindex') === '0' &&",
    "    region.scrollWidth > region.clientWidth;",
    "})()"
  )))

  # Reporting controls expand evenly in the stacked mobile state.
  js("$('.main-sidebar [data-value=\"tab_reporting\"]').first().click(); true")
  wait_for("$('#app-reporting-run_report:visible').length === 1",
           label = "Reporting filters visible")
  expect_true(js(paste(
    "(function() {",
    "  var controls = Array.from(document.querySelectorAll(",
    "    '.rids-reporting-filters .form-group, .rids-reporting-action'",
    "  ));",
    "  if (controls.length < 5) return false;",
    "  var widths = controls.map(function(control) {",
    "    return Math.round(control.getBoundingClientRect().width);",
    "  });",
    "  return document.documentElement.scrollWidth <= window.innerWidth &&",
    "    Math.max.apply(null, widths) - Math.min.apply(null, widths) <= 1;",
    "})()"
  )))
})
