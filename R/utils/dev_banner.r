banner_config <- function(status = get0("APP_STATUS", ifnotfound = "live", inherits = TRUE)) {
  status <- tolower(trimws(as.character(status %||% "live")))

  if (identical(status, "dev")) {
    return(list(
      text = paste(
        "This is a development build —",
        "Features may be incomplete and subject to change.",
        "Please report any issues."
      ),
      background = "#fdf6e3",
      color = "#5c4a1a",
      border = "#d49434"
    ))
  }

  if (identical(status, "test")) {
    return(list(
      text = "This is the RIDS test environment. Use it for validation and pre-release checks.",
      background = "#eef4ff",
      color = "#1f4f82",
      border = "#5b8def"
    ))
  }

  list(
    text = "🎉 This is the live RIDS application. Data entered and outputs generated here are part of the active production service.",
    background = "#edf7ed",
    color = "#1f5f3b",
    border = "#49a56b"
  )
}

dev_banner <- function() {
  cfg <- banner_config()

  div(
    class = "dev-banner",
    style = paste(
      sprintf("background: %s;", cfg$background),
      sprintf("color: %s;", cfg$color),
      sprintf("border-left: 4px solid %s;", cfg$border),
      "border-radius: 0 4px 4px 0;",
      "padding: 0.5rem 1rem;",
      "font-size: 0.82rem;",
      "letter-spacing: 0.01em;",
      "max-width: 720px;",
      "margin: 0.75rem auto;",
      "z-index: 1050;"
    ),
    cfg$text
  )
}
