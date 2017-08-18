# trying to write some tests for my draw_proteins package
# my best schematic as of 20170630 only uses two functions
# extract_feat_acc
# and the phosphosite extraction function.

# the extract_feat_acc function works inside a loop which is NOT great.

# it works with multiple accession numbers and with one I think?


# write function to do unit tests with is extract_feat_acc
context("features_to_dataframe")

test_that("features_to_dataframe",{

  # load data from the package
  data("five_rel_list")
  # five_rel_list was created 20170818 using this code:
  # "Q04206 Q01201 Q04864 P19838 Q00653" %>%
  #   drawProteins::get_features() ->
  #   five_rel_list

  prot_data <- feature_to_dataframe(five_rel_list)

  expect_is(prot_data, "data.frame")

  # ncol at this point 9
  expect_equal(ncol(prot_data), 9)   # exact for sample data

  expect_equal(nrow(prot_data), 319)  # exact for sample data
  expect_match(prot_data[1,1], "CHAIN") # first element should be "CHAIN"
  expect_match(colnames(prot_data)[3], "begin") # 3rd column name is begin
  expect_match(colnames(prot_data)[4], "end") # 4th column name is end
  expect_match(colnames(prot_data)[5], "length") # 5th column name is length
  expect_match(colnames(prot_data)[6], "accession") # 6th column name is accession
  expect_match(colnames(prot_data)[7], "entryName") # 7th column name is entryName
  expect_match(colnames(prot_data)[8], "taxid") # 8th column name is taxid
  expect_match(colnames(prot_data)[9], "order") # 9th column name is order

})


# write function to do unit tests with is extract_feat_acc
context("extract_feat_acc")

test_that("extract_feat_acc works to give ",{

  # load data from the package - should I move it into the test folder
  data("rel_A_features")  # this is object created from whole Uniprot GET
  # creates object protein_json in the environment.
  # it's the json data for Q04206 - for the transcription factor RelA

  # Nice simple test
  res <- extract_feat_acc(rel_A_features)

  expect_is(res, "data.frame") # generic

  # ncol at this point only 8 as order is added later.
  expect_equal(ncol(res), 8)   # exact for sample data

  expect_equal(nrow(res), 75)  # exact for sample data
  expect_match(res[1,1], "CHAIN") # first element should be "CHAIN"
  expect_match(colnames(res)[3], "begin") # 3rd column name is begin
  expect_match(colnames(res)[4], "end") # 4th column name is end
  expect_match(colnames(res)[6], "accession") # 6th column name is accession

  # need to add tests to check structure is going to work for plotting...

})
