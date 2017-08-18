# unit tests with is get_features
context("get_features")

# test only works if there is an internet connection

test_that("get_features",{

  # load data from the package
  data("prot_data")
  # prot_data was created 20170818 using this code:
  # "Q04206 Q01201 Q04864 P19838 Q00653" %>%
  #   drawProteins::get_features() %>%
  #   drawProteins::feature_to_dataframe() ->
  #   prot_data

  # Nice simple test
  res <- get_features("Q04206 Q01201 Q04864 P19838 Q00653")

  expect_is(res, "list")
  expect_equal(length(res), 5)
  expect_equal(prot_data, feature_to_dataframe(res))
})
