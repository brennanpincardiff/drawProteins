# unit tests with is get_features
context("get_features")

# test only works if there is an internet connection

test_that("get_features",{

  # load data from the package
  data("five_rel_data")
  # prot_data was created 20171101 using this code:
  # "Q04206 Q01201 Q04864 P19838 Q00653" %>%
  #   drawProteins::get_features() %>%
  #   drawProteins::feature_to_dataframe() ->
  #   five_rel_data

  # Nice simple test
  res <- get_features("Q04206 Q01201 Q04864 P19838 Q00653")

  expect_is(res, "list")
  expect_equal(length(res), 5)

  res2 <- get_features("")
  expect_is(res2, "list")
  expect_equal(res2$requestedURL,
"https://www.ebi.ac.uk/proteins/api/features?offset=0&size=100&accession=")
  expect_equal(names(res2[2]), "errorMessage")
})
