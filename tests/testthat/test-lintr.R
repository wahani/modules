test_that("Package Style", {
  if (requireNamespace("lintr", quietly = TRUE)) {
    lintr::expect_lint_free(linters = list(
      a = lintr::assignment_linter,
      b = lintr::commas_linter,
      ## c = lintr::commented_code_linter,
      d = lintr::infix_spaces_linter,
      e = lintr::line_length_linter(100),
      f = lintr::no_tab_linter,
      ## g = lintr::snake_case_linter, # detects only testthat functions
      h = lintr::object_length_linter(),
      i = lintr::spaces_left_parentheses_linter
   ))
 }
})
