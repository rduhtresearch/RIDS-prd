load_amendment_template_name_dependencies <- function() {
  source_from_root(
    "R/utils/amendment_workflow_ui.R",
    "R/utils/amendment_template_names.R"
  )
}

edge_template_fixture <- function(name = "Main Arm") {
  stats::setNames(
    list(data.frame(
      `Template Name` = c(name, name),
      `Analysis Code` = c("EDGE-0001", "EDGE-0002"),
      Description = c("Visit 1", "Visit 2"),
      check.names = FALSE,
      stringsAsFactors = FALSE
    )),
    name
  )
}

test_that("baseline EDGE template names are unchanged", {
  load_amendment_template_name_dependencies()
  templates <- edge_template_fixture()

  expect_identical(
    suffix_amendment_template_names(templates, "baseline", NULL),
    templates
  )
})

test_that("substantial amendment suffix is applied to files and EDGE names", {
  load_amendment_template_name_dependencies()
  templates <- edge_template_fixture()
  expected <- "Main Arm [SUBSTANTIAL AMENDMENT - 10 Jul 2026]"

  suffixed <- suffix_amendment_template_names(
    templates,
    "substantial_amendment",
    as.Date("2026-07-10")
  )

  expect_identical(names(suffixed), expected)
  expect_true(all(suffixed[[1]][["Template Name"]] == expected))
  expect_identical(suffixed[[1]]$Description, templates[[1]]$Description)
  expect_identical(
    suffix_amendment_template_names(
      suffixed,
      "substantial_amendment",
      as.Date("2026-07-10")
    ),
    suffixed
  )
})

test_that("distribution amendment suffix includes its effective date", {
  load_amendment_template_name_dependencies()
  templates <- edge_template_fixture("Pharmacy")
  expected <- "Pharmacy [DISTRIBUTION AMENDMENT - 01 Aug 2026]"

  suffixed <- suffix_amendment_template_names(
    templates,
    "distribution_amendment",
    as.Date("2026-08-01")
  )

  expect_identical(names(suffixed), expected)
  expect_true(all(suffixed[[1]][["Template Name"]] == expected))
})

test_that("amendment export fails closed without a valid date", {
  load_amendment_template_name_dependencies()

  expect_error(
    suffix_amendment_template_names(
      edge_template_fixture(),
      "substantial_amendment",
      NA
    ),
    "valid effective-from date"
  )
})

test_that("amendment CSV filenames retain readable bracketed suffixes", {
  load_amendment_template_name_dependencies()
  name <- "Main Arm [SUBSTANTIAL AMENDMENT - 10 Jul 2026]"

  expect_identical(
    edge_template_export_stem(name, "substantial_amendment"),
    name
  )
  expect_identical(
    edge_template_export_stem(
      "Main/Arm [DISTRIBUTION AMENDMENT - 01 Aug 2026]",
      "distribution_amendment"
    ),
    "Main_Arm [DISTRIBUTION AMENDMENT - 01 Aug 2026]"
  )
})

test_that("baseline CSV filename sanitisation remains unchanged", {
  load_amendment_template_name_dependencies()

  expect_identical(
    edge_template_export_stem("Main Arm [Original]", "baseline"),
    "Main_Arm__Original_"
  )
})

test_that("amendment analysis codes carry the study template version number", {
  load_amendment_template_name_dependencies()
  templates <- edge_template_fixture()

  qualified <- qualify_amendment_analysis_codes(
    templates,
    "substantial_amendment",
    3L
  )

  expect_identical(
    qualified[[1]][["Analysis Code"]],
    c("V3-EDGE-0001", "V3-EDGE-0002")
  )
  expect_identical(
    qualify_amendment_analysis_codes(qualified, "substantial_amendment", 3L),
    qualified
  )
})

test_that("baseline analysis codes remain backward compatible", {
  load_amendment_template_name_dependencies()
  templates <- edge_template_fixture()

  expect_identical(
    qualify_amendment_analysis_codes(templates, "baseline", NULL),
    templates
  )
})

test_that("versioned EDGE analysis codes split into safe join values", {
  load_amendment_template_name_dependencies()

  parsed <- parse_versioned_edge_analysis_code(
    c("V3-EDGE-0007", "EDGE-0007", NA_character_)
  )

  expect_identical(parsed$version_number, c(3L, NA_integer_, NA_integer_))
  expect_identical(parsed$edge_key, c("EDGE-0007", "EDGE-0007", NA_character_))
})

test_that("amendment analysis codes fail closed without join metadata", {
  load_amendment_template_name_dependencies()

  expect_error(
    qualify_amendment_analysis_codes(
      edge_template_fixture(),
      "distribution_amendment",
      NA_integer_
    ),
    "valid template version number"
  )

  missing_code <- edge_template_fixture()
  missing_code[[1]][["Analysis Code"]] <- NULL
  expect_error(
    qualify_amendment_analysis_codes(missing_code, "distribution_amendment", 2L),
    "Analysis Code column"
  )
})
