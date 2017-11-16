# writing tests for function to extract multiple transcripts

# there are loops in the function which show my limited programming skills
# but the function seems to work and addresses a significant limitation of the
# package

# write function to do unit tests with is extract_feat_acc
context("extract_transcripts")

test_that("extract_transcripts",{

  # load data from the package
  data("five_rel_data")
  # data for the five human NF-kappaB proteins

  new_data <- drawProteins::extract_transcripts(five_rel_data)

  expect_is(new_data, "data.frame")
  expect_equal(nrow(new_data), 430)
  expect_equal(ncol(new_data), 9)
  expect_equal(max(new_data$order), 7)

})

