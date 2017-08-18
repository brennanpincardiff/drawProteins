# trying to write some tests for my draw_proteins package

# unit tests with geom_chain
context("geom_chain")

test_that("geom_chains",{

  # load data from the package
  data("prot_data")
  # prot_data was created 20170818 using this code:
  # "Q04206 Q01201 Q04864 P19838 Q00653" %>%
  #   drawProteins::get_features() %>%
  #   drawProteins::feature_to_dataframe() ->
  #   prot_data

  # prot_data is a dataframe - 319 obs of 9 variables.
  p <- geom_chains(prot_data)

  # p is a ggplot object and as such is a list of 9
  expect_is(p,"ggplot")
  expect_equal(mode(p), "list")
  expect_equal(length(p), 9)

  # p should have two layers at this point
  expect_equal(length(p$layers), 2)

# https://stackoverflow.com/questions/13457562/how-to-determine-the-geom-type-of-each-layer-of-a-ggplot2-object/43982598#43982598
  # types of layers...
  expect_equal(class(p$layers[[1]]$geom)[1], "GeomRect")
  expect_equal(class(p$layers[[2]]$geom)[1], "GeomText")

  # p should have some labels
  expect_equal(p$labels$xmin, "begin")
  expect_equal(p$labels$xmax, "end")
  expect_equal(p$labels$ymin, "order - 0.2")
  expect_equal(p$labels$x, "x")
  expect_equal(p$labels$y, "y")
  expect_equal(length(prot_data[prot_data$type == "DOMAIN",]),
               length(p$layers[[1]]$data))
})



# unit tests with geom_domains
context("geom_domains")

test_that("geom_domains",{

  # load data from the package
  data("prot_data")
  # prot_data was created 20170818 using this code:
  # "Q04206 Q01201 Q04864 P19838 Q00653" %>%
  #   drawProteins::get_features() %>%
  #   drawProteins::feature_to_dataframe() ->
  #   prot_data

  # prot_data is a dataframe - 319 obs of 9 variables.
  p <- geom_chains(prot_data)
  p <- geom_domains(p)

  # p is a ggplot object and as such is a list of 9
  expect_equal(mode(p), "list")
  expect_equal(length(p), 9)

  # p should have four layers at this point
  expect_equal(length(p$layers), 4)
  # two from geom_chains and two from geom_domains

  expect_equal(length(prot_data[prot_data$type == "DOMAIN",]),
               length(p$layers[[3]]$data))

  # types of layers, 3 and 4 added by geom_domains
  expect_equal(class(p$layers[[3]]$geom)[1], "GeomRect")
  expect_equal(class(p$layers[[4]]$geom)[1], "GeomLabel")

})




# unit tests with geom_phospho
context("geom_phospho")

test_that("geom_phospho",{

  # load data from the package
  data("prot_data")
  # prot_data was created 20170818 using this code:
  # "Q04206 Q01201 Q04864 P19838 Q00653" %>%
  #   drawProteins::get_features() %>%
  #   drawProteins::feature_to_dataframe() ->
  #   prot_data

  # prot_data is a dataframe - 319 obs of 9 variables.
  p <- geom_chains(prot_data)
  p <- geom_phospho(p)

  # p is a ggplot object and as such is a list of 9
  expect_equal(mode(p), "list")
  expect_equal(length(p), 9)

  # p should have three layers at this point
  expect_equal(length(p$layers), 3)
  # two from geom_chains and one from geom_phospho
  # layers, 3 and 4 added by geom_domains
  expect_equal(class(p$layers[[3]]$geom)[1], "GeomPoint")

  # should be 32 phosphorylation sites across data set...
  expect_equal(32, nrow(p$layers[[3]]$data))

  })


# useful advice here:
# https://stackoverflow.com/questions/31038709/how-to-write-a-test-for-a-ggplot-plot
