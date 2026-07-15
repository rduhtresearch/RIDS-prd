test_that("amendment workflow banner is absent from baseline processing", {
  source_from_root("R/utils/amendment_workflow_ui.R")

  expect_null(amendment_workflow_banner("baseline", NULL, "Study", "12345"))
  expect_null(amendment_workflow_banner(NULL, NULL, "Study", "12345"))
})

test_that("amendment workflow banner identifies type, date, and study", {
  source_from_root("R/utils/amendment_workflow_ui.R")

  banner <- amendment_workflow_banner(
    "substantial_amendment",
    as.Date("2026-07-10"),
    "Example Study",
    "12345"
  )
  html <- htmltools::renderTags(banner)$html

  expect_match(html, "Substantial amendment", fixed = TRUE)
  expect_match(html, "10 Jul 2026", fixed = TRUE)
  expect_match(html, "Example Study · CPMS 12345", fixed = TRUE)
  expect_match(html, "amendment-workflow-banner", fixed = TRUE)
})

test_that("distribution amendments use their own label", {
  source_from_root("R/utils/amendment_workflow_ui.R")

  banner <- amendment_workflow_banner(
    "distribution_amendment",
    as.Date("2026-08-01")
  )
  html <- htmltools::renderTags(banner)$html

  expect_match(html, "Distribution amendment", fixed = TRUE)
  expect_match(html, "01 Aug 2026", fixed = TRUE)
})

test_that("posting line version choices include completed versions only", {
  source_from_root("R/utils/amendment_workflow_ui.R")

  versions <- data.frame(
    version_id = c(11L, 12L, 13L, 14L),
    version_number = 1:4,
    version_type = c(
      "baseline",
      "substantial_amendment",
      "distribution_amendment",
      "substantial_amendment"
    ),
    effective_from_date = as.Date(c(NA, "2026-07-10", "2026-08-01", "2026-09-01")),
    status = c("active", "active", "archived", "processing")
  )

  choices <- template_version_choices(versions)

  expect_equal(unname(choices), c("11", "12", "13"))
  expect_equal(
    names(choices),
    c(
      "Version 1 - Original template",
      "Version 2 - SUBSTANTIAL AMENDMENT - 10 Jul 2026",
      "Version 3 - DISTRIBUTION AMENDMENT - 01 Aug 2026 - ARCHIVED"
    )
  )
})

test_that("posting line version defaults to resolved or latest completed version", {
  source_from_root("R/utils/amendment_workflow_ui.R")

  versions <- data.frame(
    version_id = c(11L, 12L, 13L),
    version_number = 1:3,
    version_type = c("baseline", "substantial_amendment", "distribution_amendment"),
    effective_from_date = as.Date(c(NA, "2026-07-10", "2026-08-01")),
    status = c("active", "active", "processing")
  )

  expect_equal(default_template_version_id(versions, versions[1, ]), "11")
  expect_equal(default_template_version_id(versions, versions[3, ]), "12")
  expect_null(default_template_version_id(versions[versions$status == "processing", ]))
})

test_that("posting line filenames identify the template version", {
  source_from_root("R/utils/amendment_workflow_ui.R")

  baseline <- data.frame(
    version_number = 1L,
    version_type = "baseline",
    effective_from_date = as.Date(NA)
  )
  amendment <- data.frame(
    version_number = 2L,
    version_type = "substantial_amendment",
    effective_from_date = as.Date("2026-07-10")
  )

  expect_equal(template_version_filename_token(baseline), "v1_original")
  expect_equal(
    template_version_filename_token(amendment),
    "v2_substantial_amendment_20260710"
  )
})
