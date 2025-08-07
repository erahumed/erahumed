test_that("parse_docs_yml() returns a list", {
  expect_vector(parse_docs_yml(), ptype = list())
})
