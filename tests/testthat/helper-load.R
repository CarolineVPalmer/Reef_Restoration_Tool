# Load the tool's functions for the tests. testthat runs tests with the working
# directory set to tests/testthat, so the project folder is two levels up.
project_dir <- normalizePath(file.path("..", ".."))
for (f in sort(list.files(file.path(project_dir, "R"), pattern = "[.]R$", full.names = TRUE))) source(f)
example_path <- file.path(project_dir, "data", "example_transects.csv")
