test_that("prepare_display_table creates expected columns", {
  # Create minimal test data matching the raw post format
  test_posts <- dplyr::tibble(
    Post = "Hello world",
    By = "test.bsky.social",
    Name = "Test User",
    CreatedAt = as.POSIXct("2026-03-15 12:00:00", tz = "UTC"),
    Likes = 5L,
    Reposts = 2L,
    Replies = 1L,
    URI = "at://did:plc:abc/app.bsky.feed.post/123",
    ExternalURLs = "https://example.com",
    Tags = list(c("rstats", "conference")),
    PostID = "123",
    URL = "https://bsky.app/profile/test.bsky.social/post/123",
    Text = "Hello world <a target='_blank' href='https://bsky.app/profile/test.bsky.social/post/123'> <strong> >> </strong></a>",
    TimePulled = Sys.time(),
    Tags_String = "rstats, conference"
  )

  result <- prepare_display_table(test_posts)

  expect_s3_class(result, "data.table")
  expect_true("CreatedAt" %in% names(result))
  expect_true("Text" %in% names(result))
  expect_true("Author" %in% names(result))
  expect_true("Tags" %in% names(result))
  expect_true("AllTags" %in% names(result))
  expect_true("HasExternalURLs" %in% names(result))
  expect_true("CreatedDate" %in% names(result))
  expect_equal(nrow(result), 1)
})

test_that("prepare_display_table filters by min_date", {
  test_posts <- dplyr::tibble(
    Post = c("Old post", "New post"),
    By = rep("test.bsky.social", 2),
    Name = rep("Test User", 2),
    CreatedAt = as.POSIXct(c("2025-06-01 12:00:00", "2026-03-15 12:00:00"), tz = "UTC"),
    Likes = c(1L, 5L),
    Reposts = c(0L, 2L),
    Replies = c(0L, 1L),
    URI = c("at://1", "at://2"),
    ExternalURLs = c(NA_character_, "https://example.com"),
    Tags = list(c("old"), c("new")),
    PostID = c("1", "2"),
    URL = c("https://bsky.app/1", "https://bsky.app/2"),
    Text = c("Old post >>", "New post >>"),
    TimePulled = rep(Sys.time(), 2),
    Tags_String = c("old", "new")
  )

  result <- prepare_display_table(test_posts, min_date = as.Date("2026-01-01"))
  expect_equal(nrow(result), 1)
  expect_equal(result$Author, "Test User")
})

test_that("prepare_display_table handles HasExternalURLs correctly", {
  test_posts <- dplyr::tibble(
    Post = c("No URL here", "Has embed URL", "URL in text https://example.com",
             "Bare domain tinyurl.com/abc123"),
    By = rep("test.bsky.social", 4),
    Name = rep("Test User", 4),
    CreatedAt = rep(as.POSIXct("2026-03-15 12:00:00", tz = "UTC"), 4),
    Likes = c(1L, 2L, 3L, 4L),
    Reposts = rep(0L, 4),
    Replies = rep(0L, 4),
    URI = paste0("at://", 1:4),
    ExternalURLs = c(NA_character_, "https://example.com", NA_character_, NA_character_),
    Tags = list(NA, c("tag1"), c("tag2"), c("tag3")),
    PostID = as.character(1:4),
    URL = paste0("https://bsky.app/", 1:4),
    Text = paste0(c("No URL here", "Has embed URL", "URL in text", "Bare domain"), " >>"),
    TimePulled = rep(Sys.time(), 4),
    Tags_String = c(NA, "tag1", "tag2", "tag3")
  )

  result <- prepare_display_table(test_posts)
  expect_equal(result$HasExternalURLs, c(FALSE, TRUE, TRUE, TRUE))
})
