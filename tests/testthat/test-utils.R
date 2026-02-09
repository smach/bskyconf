test_that("tags_to_links converts tags to HTML links", {
  result <- tags_to_links(c("rstats", "positconf"))
  expect_match(result, "rstats")
  expect_match(result, "positconf")
  expect_match(result, "https://bsky.app/hashtag/rstats")
  expect_match(result, "<a href=")
  expect_match(result, ", ")
})

test_that("tags_to_links returns NA for NA input", {
  expect_true(is.na(tags_to_links(NA)))
})

test_that("tags_to_links returns NA for empty input", {
  expect_true(is.na(tags_to_links(character(0))))
})

test_that("tags_to_links handles single tag", {
  result <- tags_to_links("rstats")
  expect_match(result, "rstats")
  expect_no_match(result, ",")
})

test_that("make_slug creates valid slugs", {
  expect_equal(make_slug("NICAR 2026"), "nicar_2026")
  expect_equal(make_slug("posit::conf(2025)"), "posit_conf_2025")
  expect_equal(make_slug("My Cool Conference!"), "my_cool_conference")
})

test_that("make_slug handles edge cases", {
  expect_equal(make_slug("simple"), "simple")
  expect_equal(make_slug("  spaces  "), "spaces")
  expect_equal(make_slug("a--b__c"), "a_b_c")
})
