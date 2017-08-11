# trying to write some tests for my draw_proteins package

# first function to write tests with is extract_names
context("extract_names")

test_that("extract_names gives a list",{

  # load data from the package - should I move it into the test folder
  data("protein_json")
  # creates object protein_json in the environment.
  # it's the json data for Q04206 - for the transcription factor RelA

  # Nice simple test
  res <- extract_names(protein_json)
  expect_is(res, "list")
  expect_equal(length(res),6)

})
