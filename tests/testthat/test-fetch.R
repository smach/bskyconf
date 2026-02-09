test_that("extract_author_info handles nested author list column", {
  # Simulate bskyr >= 0.4.0 response with nested author tibbles
  mock_data <- dplyr::tibble(
    uri = c("at://did:plc:abc/app.bsky.feed.post/1",
            "at://did:plc:def/app.bsky.feed.post/2"),
    author = list(
      dplyr::tibble(handle = "alice.bsky.social", display_name = "Alice"),
      dplyr::tibble(handle = "bob.bsky.social", display_name = "Bob")
    )
  )

  result <- extract_author_info(mock_data, names(mock_data))
  expect_equal(result$handle, c("alice.bsky.social", "bob.bsky.social"))
  expect_equal(result$display_name, c("Alice", "Bob"))
})

test_that("extract_author_info handles flat author columns", {
  # Simulate older bskyr response with flat columns
  mock_data <- dplyr::tibble(
    uri = c("at://1", "at://2"),
    author_handle = c("alice.bsky.social", "bob.bsky.social"),
    author_display_name = c("Alice", "Bob")
  )

  result <- extract_author_info(mock_data, names(mock_data))
  expect_equal(result$handle, c("alice.bsky.social", "bob.bsky.social"))
  expect_equal(result$display_name, c("Alice", "Bob"))
})

test_that("extract_external_url handles various embed structures", {
  # Flat external_uri (bskyr tibble embed)
  embed_flat <- dplyr::tibble(external_uri = "https://example.com")
  expect_equal(extract_external_url(embed_flat), "https://example.com")

  # Nested external$uri (raw list embed)
  embed_nested <- list(external = list(uri = "https://example.com"))
  expect_equal(extract_external_url(embed_nested), "https://example.com")

  # Image-only embed (no external URL)
  embed_image <- dplyr::tibble(images = list(list()))
  expect_equal(extract_external_url(embed_image), NA_character_)

  # NULL embed
  expect_equal(extract_external_url(NULL), NA_character_)
})

test_that("extract_tags extracts from top-level tags field", {
  record <- list(tags = c("rstats", "dataviz"), text = "hello")
  expect_equal(extract_tags(record), c("rstats", "dataviz"))
})

test_that("extract_tags extracts from facets when tags field is missing", {
  record <- list(
    text = "Hello #rstats",
    facets = list(
      list(
        list(
          features = list(
            list(`$type` = "app.bsky.richtext.facet#tag", tag = "rstats")
          ),
          index = list(byteStart = 6L, byteEnd = 13L)
        )
      )
    )
  )
  expect_equal(extract_tags(record), "rstats")
})

test_that("extract_tags returns empty character for record with no tags", {
  record <- list(text = "Hello world")
  expect_equal(extract_tags(record), character(0))
  expect_equal(extract_tags(NULL), character(0))
})

test_that("at_uri_to_url converts AT URIs to Bluesky web URLs", {
  uris <- c(
    "at://did:plc:abc123/app.bsky.feed.post/post1",
    "at://did:plc:def456/app.bsky.feed.post/post2"
  )
  expected <- c(
    "https://bsky.app/profile/did:plc:abc123/post/post1",
    "https://bsky.app/profile/did:plc:def456/post/post2"
  )
  expect_equal(at_uri_to_url(uris), expected)
})

test_that("at_uri_to_url handles single URI", {
  uri <- "at://did:plc:abc/app.bsky.feed.post/xyz"
  expect_equal(at_uri_to_url(uri), "https://bsky.app/profile/did:plc:abc/post/xyz")
})

test_that("empty_posts_tibble returns correct structure", {
  result <- empty_posts_tibble()
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0)
  expected_cols <- c("Post", "By", "Name", "CreatedAt", "Likes", "Reposts",
                     "Replies", "URI", "ExternalURLs", "Tags", "PostID",
                     "URL", "Text", "TimePulled", "Tags_String")
  expect_equal(names(result), expected_cols)
})

test_that("find_column finds exact matches", {
  cols <- c("uri", "author_handle", "like_count")
  expect_equal(find_column(cols, c("author_handle", "handle")), "author_handle")
  expect_equal(find_column(cols, c("uri")), "uri")
})

test_that("find_column falls back to partial matching", {
  cols <- c("uri", "author_handle", "like_count")
  expect_equal(find_column(cols, c("handle")), "author_handle")
})

test_that("find_column errors when no match found", {
  cols <- c("uri", "author", "like_count")
  expect_error(
    find_column(cols, c("nonexistent_col")),
    "Could not find"
  )
})
