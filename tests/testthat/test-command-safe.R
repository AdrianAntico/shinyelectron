test_that("run_command_safe does not spawn an executable that is missing", {
  skip_if_not_installed("mockery")
  spawned <- FALSE
  mockery::stub(run_command_safe, "processx::run", function(...) {
    spawned <<- TRUE
    stop("processx must not be called")
  })

  result <- run_command_safe("definitely-not-installed-shinyelectron.exe")

  expect_false(spawned)
  expect_identical(result$status, 127L)
  expect_match(result$stderr, "Command not found")
})

test_that("run_command_safe still delegates existing executables to processx", {
  skip_if_not_installed("mockery")
  executable <- Sys.which(if (.Platform$OS.type == "windows") "cmd.exe" else "sh")
  skip_if(!nzchar(executable), "No test shell is available")

  spawned <- FALSE
  mockery::stub(run_command_safe, "processx::run", function(...) {
    spawned <<- TRUE
    list(status = 0L, stdout = "ok", stderr = "")
  })

  result <- run_command_safe(unname(executable))

  expect_true(spawned)
  expect_identical(result$status, 0L)
})
