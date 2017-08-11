# trying to write some tests for my draw_proteins package
# my best schematic as of 20170630 only uses two functions
# extract_feat_acc
# and the phosphosite extraction function.

# the function works inside a loop which is NOT great.

# it works with multiple accession numbers but NOT with one I think?


# write function to do unit tests with is extract_feat_acc
context("extract_feat_acc")

test_that("extract_feat_acc works to give ",{

  # load data from the package - should I move it into the test folder
  data("rel_A_features")  # this is object created from whole Uniprot GET
  # creates object protein_json in the environment.
  # it's the json data for Q04206 - for the transcription factor RelA

  # Nice simple test
  res <- extract_feat_acc(rel_A_features)

  ### N.B. This gives 45 warnings!!! NOT good!! WHY????
  ### going to get me in trouble
  # have to stop now but this is important to remember...
  # fiftieth element is an example

  # reason for this is that some of the features DON'T HAVE DESCRIPTIONS!!!
  # SO WHEN LOOKING FOR DESCRIPTIONS - NONE RETURNED!!!

  # GOING TO NEED AN IF STATEMENT FOR THIS, I THINK....

  expect_is(res, "data.frame")
  # expect_equal(res, ncol,7)
  # expect_equal(res, nrow,75)

  # should contain a type of "CHAIN"
  # should have accession number, entryName
  # need to add tests to check structure is going to work for plotting...

})
