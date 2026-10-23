# Load the pipeline functions for the tests (the project is not a package).
# Run all tests from the repo root with: testthat::test_dir("tests/testthat")
root <- normalizePath(file.path(testthat::test_path(), "..", ".."))
for (f in list.files(file.path(root, "R"), pattern = "\\.R$", full.names = TRUE)) source(f, local = FALSE)
raw_path <- function(...) file.path(root, "data-raw", ...)
