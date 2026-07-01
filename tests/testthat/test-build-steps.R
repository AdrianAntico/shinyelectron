# tests/testthat/test-build-steps.R

test_that("dist_has_platform_artifact detects a finished installer", {
  tmp <- withr::local_tempdir()
  dist <- fs::dir_create(fs::path(tmp, "dist"))
  fs::file_create(fs::path(dist, "MyApp-1.0.0-arm64.dmg"))
  expect_true(dist_has_platform_artifact(tmp, "mac"))
})

test_that("dist_has_platform_artifact ignores the unpacked app directory", {
  tmp <- withr::local_tempdir()
  dist <- fs::dir_create(fs::path(tmp, "dist"))
  # electron-builder leaves the unpacked .app under mac-arm64/ even when the
  # installer step (signing, notarization, dmg packaging) failed. That must
  # not read as success, or the collect step publishes nothing.
  fs::dir_create(fs::path(dist, "mac-arm64", "MyApp.app"))
  expect_false(dist_has_platform_artifact(tmp, "mac"))
})

test_that("dist_has_platform_artifact returns FALSE when dist is absent", {
  tmp <- withr::local_tempdir()
  expect_false(dist_has_platform_artifact(tmp, "mac"))
})
