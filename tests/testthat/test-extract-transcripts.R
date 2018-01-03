# unit tests for extract_transcripts
context("extract_transcripts")

test_that("extract_transcripts",{

  # load data from the package
  data("five_rel_data")
  # prot_data was created 20171101 using this code:
  # "Q04206 Q01201 Q04864 P19838 Q00653" %>%
  #   drawProteins::get_features() %>%
  #   drawProteins::feature_to_dataframe() ->
  #   five_rel_data

  # Nice simple test
  new_data <- extract_transcripts(five_rel_data)

  expect_is(new_data, "data.frame")
  # it should be longer than five_rel_data as two extra chains added
  expect_equal(dim(new_data)[1], 430)  # rows
  expect_equal(dim(new_data)[2], 9)    # columns
  expect_equal(max(new_data$order), 7) # should be 7 chains to plot
})
