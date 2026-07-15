source_from_root("R/modules/reporting_mod.R")

test_that("Cost Events query omits blank filters and preserves API names", {
  query <- edge_cost_events_query(2026, NA, "", "P03")

  expect_equal(query, list(Year = "2026", Period = "P03"))
})

test_that("Cost Events component pickers extract independent API values", {
  filters <- edge_cost_events_picker_filters(
    year = as.Date("2026-04-01"),
    month = as.Date("2025-07-01"),
    day = as.Date("2025-01-15")
  )

  expect_equal(filters, list(year = "2026", month = "7", day = "15"))
  expect_equal(edge_cost_events_picker_filters(), list(year = NULL, month = NULL, day = NULL))
})

test_that("Cost Events component pickers reject invalid values", {
  expect_error(edge_cost_events_picker_value("invalid", "year"), "valid year")
  expect_error(edge_cost_events_picker_value(Sys.Date(), "quarter"), "Unsupported")
})

test_that("Cost Events range validates and preserves exact boundaries", {
  range <- edge_cost_events_range(as.Date("2025-04-15"), as.Date("2026-03-10"))

  expect_equal(range$from, as.Date("2025-04-15"))
  expect_equal(range$to, as.Date("2026-03-10"))
  expect_equal(edge_cost_events_range_years(range), c("2025", "2026"))
})

test_that("Cost Events range validates boundaries and size", {
  expect_null(edge_cost_events_range())
  expect_error(edge_cost_events_range(Sys.Date(), NULL), "both From and To")
  expect_error(edge_cost_events_range(as.Date("2026-02-01"), as.Date("2026-01-01")), "on or before")
})

test_that("Cost Events payloads flatten into a combined stable schema", {
  payload <- list(
    projectCostEvents = list(list(edgeProjectId = 10, projectTitle = "Project", cost = 12.5)),
    projectSiteCostEvents = list(list(edgeProjectId = 11, edgeProjectSiteId = 20, site = "Site", cost = 25))
  )

  result <- flatten_edge_cost_events(payload)

  expect_identical(names(result), EDGE_COST_EVENT_COLUMNS)
  expect_equal(result$event_type, c("Project", "Project Site"))
  expect_true(is.na(result$edgeProjectSiteId[[1]]))
  expect_equal(result$edgeProjectSiteId[[2]], 20)
  expect_false(any(c("edgeParticipantId", "nhsNumber") %in% names(result)))
})

test_that("Cost Events empty arrays produce an empty stable table", {
  result <- flatten_edge_cost_events(list(
    projectCostEvents = list(),
    projectSiteCostEvents = list()
  ))

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)
  expect_identical(names(result), EDGE_COST_EVENT_COLUMNS)
})

test_that("Cost Events rejects malformed payloads", {
  expect_error(flatten_edge_cost_events(list(message = "not a report")), "unexpected")
  expect_error(flatten_edge_cost_events(NULL), "unexpected")
})

test_that("Cost Events maps known response errors to friendly messages", {
  expect_match(conditionMessage(edge_cost_events_error(400)), "filters", fixed = TRUE)
  expect_match(conditionMessage(edge_cost_events_error(401)), "API key", fixed = TRUE)
  expect_match(conditionMessage(edge_cost_events_error(429)), "rate limiting", fixed = TRUE)
  expect_match(conditionMessage(edge_cost_events_error(500)), "HTTP 500", fixed = TRUE)
})

test_that("Cost Events URL encodes populated query values", {
  url <- edge_cost_events_url(EDGE_PROJECT_COST_EVENTS_URL, list(Year = "2026", Period = "P 03"))

  expect_match(url, "Year=2026", fixed = TRUE)
  expect_match(url, "Period=P%2003", fixed = TRUE)
})

test_that("Cost Events request sends the API key and populated filters", {
  captured <- list()
  fake_perform <- function(url, headers, timeout) {
    captured[[length(captured) + 1L]] <<- list(url = url, headers = headers, timeout = timeout)
    response_field <- if (grepl("ProjectSiteCostEvents", url, fixed = TRUE)) {
      "projectSiteCostEvents"
    } else {
      "projectCostEvents"
    }
    list(
      status_code = 200L,
      body = sprintf('{"%s":[]}', response_field)
    )
  }

  result <- fetch_edge_cost_events(
    api_key = "secret-key",
    year = 2026,
    month = "",
    day = NA,
    period = "P03",
    perform = fake_perform
  )

  expect_equal(nrow(result), 0)
  expect_length(captured, 2)
  expect_equal(vapply(captured, function(request) request$headers$apikey, character(1)), rep("secret-key", 2))
  expect_equal(vapply(captured, function(request) request$timeout, numeric(1)), rep(30, 2))
  expect_true(all(vapply(captured, function(request) grepl("Year=2026", request$url, fixed = TRUE), logical(1))))
  expect_true(all(vapply(captured, function(request) grepl("Period=P03", request$url, fixed = TRUE), logical(1))))
  expect_false(any(vapply(captured, function(request) grepl("Month=", request$url, fixed = TRUE), logical(1))))
  expect_false(any(vapply(captured, function(request) grepl("Day=", request$url, fixed = TRUE), logical(1))))
  expect_match(captured[[1]]$url, "/ProjectCostEvents", fixed = TRUE)
  expect_match(captured[[2]]$url, "/ProjectSiteCostEvents", fixed = TRUE)
  expect_false(any(vapply(captured, function(request) grepl("Participant", request$url, fixed = TRUE), logical(1))))
})

test_that("Cost Events request raises typed errors for failed responses", {
  fake_perform <- function(url, headers, timeout) {
    list(status_code = 401L, body = "")
  }

  error <- tryCatch(
    fetch_edge_cost_events("bad-key", perform = fake_perform),
    edge_cost_events_error = identity
  )

  expect_s3_class(error, "edge_cost_events_error")
  expect_equal(error$status_code, 401L)
  expect_match(conditionMessage(error), "API key", fixed = TRUE)
})

test_that("Cost Events range uses one call per endpoint and year and trims dates", {
  captured <- character()
  fake_perform <- function(url, headers, timeout) {
    captured <<- c(captured, url)
    field <- if (grepl("ProjectSiteCostEvents", url, fixed = TRUE)) "projectSiteCostEvents" else "projectCostEvents"
    body <- sprintf(
      '{"%s":[{"edgeProjectId":1,"date":"2026-06-01T00:00:00Z"},{"edgeProjectId":2,"date":"2026-06-20T00:00:00Z"},{"edgeProjectId":3,"date":"2026-07-01T00:00:00Z"},{"edgeProjectId":4,"date":"2026-07-20T00:00:00Z"}]}',
      field
    )
    list(status_code = 200L, body = body)
  }

  result <- fetch_edge_cost_events(
    "secret-key",
    range_from = as.Date("2026-06-15"),
    range_to = as.Date("2026-07-05"),
    perform = fake_perform
  )

  expect_length(captured, 2)
  expect_true(all(vapply(c("Year=2026"), function(value) any(grepl(value, captured, fixed = TRUE)), logical(1))))
  expect_equal(sort(unique(result$date)), c("2026-06-20T00:00:00Z", "2026-07-01T00:00:00Z"))
})

test_that("Cost Events range ignores component date filters", {
  captured <- character()
  fake_perform <- function(url, headers, timeout) {
    captured <<- c(captured, url)
    field <- if (grepl("ProjectSiteCostEvents", url, fixed = TRUE)) "projectSiteCostEvents" else "projectCostEvents"
    list(status_code = 200L, body = sprintf('{"%s":[]}', field))
  }

  fetch_edge_cost_events(
    "secret-key",
    year = 1999,
    month = 12,
    day = 31,
    range_from = as.Date("2026-01-01"),
    range_to = as.Date("2026-02-01"),
    perform = fake_perform
  )

  expect_false(any(grepl("1999|Day=31|Month=12", captured)))
  expect_true(all(grepl("Year=2026", captured, fixed = TRUE)))
})

test_that("Cost Events extracts HTTP status codes from base connection errors", {
  expect_equal(edge_cost_events_http_status("cannot open URL: HTTP status was '401 Unauthorized'"), 401L)
  expect_true(is.na(edge_cost_events_http_status("connection timed out")))
})
