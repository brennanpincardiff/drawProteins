# unit tests with is parse_gff
context("parse_gff")

# test only works if there is an internet connection

test_that("parse_gff",{

  # gff file in system
  path_to_gff <- system.file("extdata", "Q04206.gff.txt", package = "drawProteins")
  gff_data <- drawProteins::parse_gff(path_to_gff)
  p <- draw_canvas(gff_data)
  p <- draw_chains(p, gff_data)
  p <- draw_folding(p, gff_data)

  expect_is(gff_data, "data.frame")
  expect_is(gff_data, "tbl")
  expect_equal(nrow(gff_data), 69)
  expect_equal(ncol(gff_data),12)
  expect_equal(colnames(gff_data)[1:5],
    c("accession","source","type","begin","end"))
  expect_equal(as.character(gff_data[1,1]), "Q04206")
  expect_equal(as.character(gff_data[1,3]), "CHAIN")
  expect_equal(colnames(gff_data)[9:12],
    c("description", "second", "order", "entryName"))
  expect_equal(as.numeric(gff_data[1,11]), 1)

  # testing p made with this data
  # p is a ggplot object and as such is a list of 9
  expect_is(p,"ggplot")
  expect_equal(mode(p), "list")
  expect_equal(length(p), 9)

  # number of layers
  expect_equal(length(p$layers), 5)

  # type of each layer
  # types of layers...
  expect_equal(class(p$layers[[1]]$geom)[1], "GeomRect")
  expect_equal(class(p$layers[[2]]$geom)[1], "GeomText")
  expect_equal(class(p$layers[[3]]$geom)[1], "GeomRect")
  expect_equal(class(p$layers[[4]]$geom)[1], "GeomRect")
  expect_equal(class(p$layers[[5]]$geom)[1], "GeomRect")


  # p should have some labels
  expect_equal(p$labels$x, "Amino acid number")
  expect_equal(p$labels$y, "")

  # type/features of data in each layer
  expect_equal(p$layers[[1]]$data$type[1], "CHAIN")
  expect_equal(as.numeric(p$layers[[2]]$data[2]), 1)
  expect_equal(p$layers[[3]]$data$type[1], "BETA STRAND")
  expect_equal(p$layers[[4]]$data$type[1], "HELIX")
  expect_equal(p$layers[[5]]$data$type[1], "TURN")

  # test for a web link
  protease_data <- parse_gff("https://www.uniprot.org/uniprot/P0A7B8.gff")
  p <- draw_canvas(protease_data)
  p <- draw_chains(p, protease_data)
  p <- draw_folding(p, protease_data)

  expect_is(protease_data, "data.frame")
  expect_is(protease_data, "tbl")
  expect_gt(nrow(protease_data), 34)
  expect_equal(ncol(protease_data), 12)
  expect_equal(colnames(protease_data)[1:5],
    c("accession","source","type","begin","end"))
  expect_equal(as.character(protease_data[1,1]), "P0A7B8")
  expect_equal(as.character(protease_data[2,3]), "CHAIN")
  expect_equal(colnames(protease_data)[9:12],
    c("description", "second", "order", "entryName"))
  expect_equal(as.numeric(protease_data[1,11]), 1)

  # then test the ggplot2 object again...
  # testing p made with this data
  # p is a ggplot object and as such is a list of 9
  expect_is(p,"ggplot")
  expect_equal(mode(p), "list")
  expect_equal(length(p), 9)

  # number of layers
  expect_equal(length(p$layers), 5)

  # type of each layer
  # types of layers...
  expect_equal(class(p$layers[[1]]$geom)[1], "GeomRect")
  expect_equal(class(p$layers[[2]]$geom)[1], "GeomText")
  expect_equal(class(p$layers[[3]]$geom)[1], "GeomRect")
  expect_equal(class(p$layers[[4]]$geom)[1], "GeomRect")
  expect_equal(class(p$layers[[5]]$geom)[1], "GeomRect")
})



