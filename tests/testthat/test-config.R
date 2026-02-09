test_that("write and read config round-trips correctly", {
  tmp_dir <- withr::local_tempdir()
  config_path <- file.path(tmp_dir, "_bskyconf.yml")

  write_conference_config(
    path = config_path,
    conference_name = "Test Conf",
    conference_slug = "test_conf",
    hashtags = c("#TestConf", "#TC2026"),
    data_dir = "data",
    posts_per_hashtag = 100L,
    min_date = as.Date("2026-01-01"),
    exclude_accounts = c("spam.bsky.social"),
    exclude_accounts_regex = "\\.bridge\\.example",
    about_text = "A test conference.",
    update_text = "Updated hourly."
  )

  expect_true(file.exists(config_path))

  config <- read_conference_config(config_path)

  expect_equal(config$conference_name, "Test Conf")
  expect_equal(config$conference_slug, "test_conf")
  expect_equal(config$hashtags, c("#TestConf", "#TC2026"))
  expect_equal(config$data_dir, "data")
  expect_equal(config$posts_per_hashtag, 100L)
  expect_equal(config$min_date, "2026-01-01")
  expect_equal(config$about_text, "A test conference.")
  expect_equal(config$update_text, "Updated hourly.")
})

test_that("read_conference_config errors on missing file", {
  expect_error(
    read_conference_config("nonexistent.yml"),
    "not found"
  )
})

test_that("read_conference_config applies defaults", {
  tmp_dir <- withr::local_tempdir()
  config_path <- file.path(tmp_dir, "_bskyconf.yml")

  yaml::write_yaml(
    list(conference_name = "Minimal", hashtags = list("#min")),
    config_path
  )

  config <- read_conference_config(config_path)

  expect_equal(config$posts_per_hashtag, 250L)
  expect_equal(config$data_dir, "data")
  expect_equal(config$exclude_accounts, character(0))
})
