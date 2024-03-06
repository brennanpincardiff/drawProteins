# unit tests for draw_chains
context("draw_canvas")
test_that("draw_canvas",{

  # load data from the package
  data("five_rel_data")
  # five_rel_data was created 20171101 using this code:
  # "Q04206 Q01201 Q04864 P19838 Q00653" %>%
  #   drawProteins::get_features() %>%
  #   drawProteins::feature_to_dataframe() ->
  #   five_rel_data

  # five_rel_data is a dataframe - 320 obs of 9 variables.
  p <- draw_canvas(five_rel_data)

  # p is a ggplot object and as such is a list of 11
  expect_is(p,"ggplot")
  expect_equal(mode(p), "list")
  expect_equal(length(p), 11)

  # p should have some labels
  expect_equal(p$labels$x, "Amino acid number")
  expect_equal(p$labels$y, "")

})


# unit tests for draw_chains
context("draw_chains")

test_that("draw_chains",{

  # load data from the package
  data("five_rel_data")
  # five_rel_data was created 20171101 using this code:
  # "Q04206 Q01201 Q04864 P19838 Q00653" %>%
  #   drawProteins::get_features() %>%
  #   drawProteins::feature_to_dataframe() ->
  #   five_rel_data

  # five_rel_data is a dataframe - 320 obs of 9 variables.
  p <- draw_canvas(five_rel_data)
  p <- draw_chains(p, five_rel_data)

  # p is a ggplot object and as such is a list of 11
  expect_is(p,"ggplot")
  expect_equal(mode(p), "list")
  expect_equal(length(p), 11)

  # p should have two layers at this point
  expect_equal(length(p$layers), 2)
# https://stackoverflow.com/questions/13457562/how-to-determine-the-geom-type-of-each-layer-of-a-ggplot2-object/43982598#43982598
  # types of layers...
  expect_equal(class(p$layers[[1]]$geom)[1], "GeomRect")
  expect_equal(class(p$layers[[2]]$geom)[1], "GeomText")
  expect_equal(length(five_rel_data[five_rel_data$type == "DOMAIN",]),
              length(p$layers[[1]]$data))
})



# unit tests with draw_domains
context("draw_domains")

test_that("draw_domains",{

  # load data from the package
  data("five_rel_data")
  # five_rel_data was created 20171101 using this code:
  # "Q04206 Q01201 Q04864 P19838 Q00653" %>%
  #   drawProteins::get_features() %>%
  #   drawProteins::feature_to_dataframe() ->
  #   five_rel_data

  # five_rel_data is a dataframe - 320 obs of 9 variables.
  p <- draw_canvas(five_rel_data)
  p <- draw_chains(p, five_rel_data)
  p <- draw_domains(p, five_rel_data)

  # p is a ggplot object and as such is a list of 11
  expect_equal(mode(p), "list")
  expect_equal(length(p), 11)

  # p should have four layers at this point
  expect_equal(length(p$layers), 4)
  # two from draw_chains and two from draw_domains

  expect_equal(length(five_rel_data[five_rel_data$type == "DOMAIN",]),
              length(p$layers[[3]]$data))

  # types of layers, 3 and 4 added by draw_domains
  expect_equal(class(p$layers[[3]]$geom)[1], "GeomRect")
  expect_equal(class(p$layers[[4]]$geom)[1], "GeomLabel")

})




# unit tests with draw_phospho
context("draw_phospho")

test_that("draw_phospho",{

  # load data from the package
  data("five_rel_data")
  # five_rel_data was created 20171101 using this code:
  # "Q04206 Q01201 Q04864 P19838 Q00653" %>%
  #   drawProteins::get_features() %>%
  #   drawProteins::feature_to_dataframe() ->
  #   five_rel_data

  # five_rel_data is a dataframe - 320 obs of 9 variables.
  p <- draw_canvas(five_rel_data)
  p <- draw_chains(p, five_rel_data)
  p <- draw_phospho(p, five_rel_data)

  # p is a ggplot object and as such is a list of 11
  expect_equal(mode(p), "list")
  expect_equal(length(p), 11)

  # p should have three layers at this point
  expect_equal(length(p$layers), 3)
  # two from draw_chains and one from draw_phospho
  # layers, 3 and 4 added by draw_domains
  expect_equal(class(p$layers[[3]]$geom)[1], "GeomPoint")

  # should be 32 phosphorylation sites across data set...
  expect_equal(32, nrow(p$layers[[3]]$data))

  })

# unit tests with draw_motif
context("draw_motif")

test_that("draw_motif",{

  # load data from the package
  data("five_rel_data")
  # five_rel_data was created 20171101 using this code:
  # "Q04206 Q01201 Q04864 P19838 Q00653" %>%
  #   drawProteins::get_features() %>%
  #   drawProteins::feature_to_dataframe() ->
  #   five_rel_data

  # five_rel_data is a dataframe - 320 obs of 9 variables.
  p <- draw_canvas(five_rel_data)
  p <- draw_chains(p, five_rel_data)
  p <- draw_motif(p, five_rel_data)

  # p is a ggplot object and as such is a list of 11
  expect_is(p,"ggplot")
  expect_equal(mode(p), "list")
  expect_equal(length(p), 11)

  # p should have three layers at this point
  expect_equal(length(p$layers), 3)
  # two from draw_chains and one from draw_motif
  expect_equal(class(p$layers[[3]]$geom)[1], "GeomRect")
  # https://stackoverflow.com/questions/13457562/how-to-determine-the-geom-type-of-each-layer-of-a-ggplot2-object/43982598#43982598
  # types of layers...
  expect_equal(class(p$layers[[1]]$geom)[1], "GeomRect")
  expect_equal(class(p$layers[[2]]$geom)[1], "GeomText")

  # p should have some labels
  expect_equal(p$labels$x, "Amino acid number")
  expect_equal(p$labels$y, "")
  expect_equal(length(five_rel_data[five_rel_data$type == "DOMAIN",]),
              length(p$layers[[1]]$data))
  # p$layers[[3]]$data contains the data that was extracted
  # dimensions are 6 9
  expect_equal(nrow(five_rel_data[five_rel_data$type == "MOTIF",]),
              nrow(p$layers[[3]]$data))
  expect_equal(p$layers[[3]]$data$type[1], "MOTIF" )
})


# unit tests with draw_regions
context("draw_regions")

test_that("draw_regions",{

  # load data from the package
  data("five_rel_data")
  # five_rel_data was created 20171101 using this code:
  # "Q04206 Q01201 Q04864 P19838 Q00653" %>%
  #   drawProteins::get_features() %>%
  #   drawProteins::feature_to_dataframe() ->
  #   five_rel_data

  # five_rel_data is a dataframe - 320 obs of 9 variables.
  p <- draw_canvas(five_rel_data)
  p <- draw_chains(p, five_rel_data)
  p <- draw_regions(p, five_rel_data)

  # p is a ggplot object and as such is a list of 11
  expect_is(p,"ggplot")
  expect_equal(mode(p), "list")
  expect_equal(length(p), 11)

  # p should have three layers at this point
  expect_equal(length(p$layers), 3)
  # two from draw_chains and one from draw_regions
  # https://stackoverflow.com/questions/13457562/how-to-determine-the-geom-type-of-each-layer-of-a-ggplot2-object/43982598#43982598
  # types of layers...
  expect_equal(class(p$layers[[1]]$geom)[1], "GeomRect")
  expect_equal(class(p$layers[[2]]$geom)[1], "GeomText")
  expect_equal(class(p$layers[[3]]$geom)[1], "GeomRect")

  # p should have some labels
  expect_equal(p$labels$x, "Amino acid number")
  expect_equal(p$labels$y, "")
  expect_equal(length(five_rel_data[five_rel_data$type == "DOMAIN",]),
              length(p$layers[[1]]$data))
  # p$layers[[3]]$data contains the data that was extracted
  # dimensions are 6 9
  expect_equal(nrow(five_rel_data[five_rel_data$type == "REGION",]),
              nrow(p$layers[[3]]$data))
  expect_equal(p$layers[[3]]$data$type[1], "REGION" )
})



# unit tests with draw_repeat
context("draw_repeat")

test_that("draw_repeat",{

  # load data from the package
  data("five_rel_data")
  # five_rel_data was created 20171101 using this code:
  # "Q04206 Q01201 Q04864 P19838 Q00653" %>%
  #   drawProteins::get_features() %>%
  #   drawProteins::feature_to_dataframe() ->
  #   five_rel_data

  # five_rel_data is a dataframe - 320 obs of 9 variables.
  p <- draw_canvas(five_rel_data)
  p <- draw_chains(p, five_rel_data)
  p <- draw_repeat(p, five_rel_data)

  # p is a ggplot object and as such is a list of 11
  expect_is(p,"ggplot")
  expect_equal(mode(p), "list")
  expect_equal(length(p), 11)

  # p should have four layers at this point
  expect_equal(length(p$layers), 4)
  # two from draw_chains and two from draw_repeat
  # first draw_repeat layer is rectanges
  expect_equal(class(p$layers[[3]]$geom)[1], "GeomRect")
  # second draw_repeat layer is text
  expect_equal(class(p$layers[[4]]$geom)[1], "GeomText")

  # p should have some labels
  expect_equal(p$labels$x, "Amino acid number")
  expect_equal(p$labels$y, "")
  expect_equal(length(five_rel_data[five_rel_data$type == "REPEAT",]),
              length(p$layers[[1]]$data))
  # p$layers[[3]]$data contains the data that was extracted
  # dimensions are 6 9
  expect_equal(nrow(five_rel_data[five_rel_data$type == "REPEAT",]),
              nrow(p$layers[[3]]$data))
  expect_equal(p$layers[[3]]$data$type[1], "REPEAT" )
})


# unit tests for draw_recept_dom
context("draw_recept_dom")

test_that("draw_recept_dom",{

  # load data from the package
  data("tnfs_data")
  # tnfs_data was created 20190103 using this code:
  # "P19438 P25942" %>%
  #   drawProteins::get_features() %>%
  #   drawProteins::feature_to_dataframe() ->
  #   tnfs_data

  # tnfs_data is a dataframe - 127 obs of 9 variables.
  p <- draw_canvas(tnfs_data)
  p <- draw_chains(p, tnfs_data)
  p <- draw_recept_dom(p, tnfs_data)

  # p is a ggplot object and as such is a list of 11
  expect_is(p,"ggplot")
  expect_equal(mode(p), "list")
  expect_equal(length(p), 11)

  # p should have four layers at this point
  # because TOPO_DOM and TRANSMEM added separately
  expect_equal(length(p$layers), 4)
  # https://stackoverflow.com/questions/13457562/how-to-determine-the-geom-type-of-each-layer-of-a-ggplot2-object/43982598#43982598
  # types of layers...
  expect_equal(class(p$layers[[1]]$geom)[1], "GeomRect")
  expect_equal(class(p$layers[[2]]$geom)[1], "GeomText")
  expect_equal(class(p$layers[[3]]$geom)[1], "GeomRect")
  expect_equal(class(p$layers[[4]]$geom)[1], "GeomRect")

  # in this case nrow of layer 3 should be 4
  # and nrow of layer 4 should be 2
  expect_equal(nrow(p$layers[[3]]$data), 4) # 2 receptors - EC and Cyto domain
  expect_equal(nrow(p$layers[[4]]$data), 2) # 2 receptors - 2 TM domain


  ## tests for label domains (default is FALSE)
  # tnfs_data is a dataframe - 127 obs of 9 variables.
  p <- draw_canvas(tnfs_data)
  p <- draw_chains(p, tnfs_data)
  p <- draw_recept_dom(p, tnfs_data, label_domains = TRUE)

  # p is a ggplot object and as such is a list of 11
  expect_is(p,"ggplot")
  expect_equal(mode(p), "list")
  expect_equal(length(p), 11)

  # p should have six layers at this point
  # because TOPO_DOM and TRANSMEM added separately
  # and labelling domains gives two more layers which are GeomLable
  expect_equal(length(p$layers), 6)
  # https://stackoverflow.com/questions/13457562/how-to-determine-the-geom-type-of-each-layer-of-a-ggplot2-object/43982598#43982598
  # types of layers...
  expect_equal(class(p$layers[[1]]$geom)[1], "GeomRect")
  expect_equal(class(p$layers[[2]]$geom)[1], "GeomText")
  expect_equal(class(p$layers[[3]]$geom)[1], "GeomRect")
  expect_equal(class(p$layers[[4]]$geom)[1], "GeomRect")
  expect_equal(class(p$layers[[5]]$geom)[1], "GeomLabel")
  expect_equal(class(p$layers[[6]]$geom)[1], "GeomLabel")

  # in this case nrow of layer 5 should be 2
  # and nrow of layer 6 should be 2
  expect_equal(nrow(p$layers[[5]]$data), 4) # 2 receptors - EC and Cyto domain
  expect_equal(nrow(p$layers[[6]]$data), 2) # 2 receptors - 2 TM domain

  # testing data and mapping of layers 5 and 6
  expect_equal(length(p$layers[[5]]$data), 9)
  expect_equal(nrow(p$layers[[5]]$data), 4)
  expect_equal(p$layers[[5]]$data$type[1], "TOPO_DOM")
  expect_equal(p$layers[[5]]$data$description[1], "Extracellular")
  expect_equal(p$layers[[5]]$data$description[2], "Cytoplasmic")
  expect_equal(p$layers[[6]]$mapping$label, "TM")
  expect_equal(p$layers[[6]]$data$type[1], "TRANSMEM")
  expect_equal(p$layers[[6]]$data$description[1], "Helical")

})


# useful advice here:
#https://stackoverflow.com/questions/31038709/how-to-write-a-test-for-a-ggplot-plot


# unit tests for draw_folding
context("draw_folding")

test_that("draw_folding",{

  # load data from the package
  data("tnfs_data")
  # tnfs_data was created 20190103 using this code:
  # "P19438 P25942" %>%
  #   drawProteins::get_features() %>%
  #   drawProteins::feature_to_dataframe() ->
  #   tnfs_data

  # tnfs_data is a dataframe - 127 obs of 9 variables.
  p <- draw_canvas(tnfs_data)
  p <- draw_chains(p, tnfs_data)
  p <- draw_folding(p, tnfs_data)

  # p is a ggplot object and as such is a list of 11
  expect_is(p,"ggplot")
  expect_equal(mode(p), "list")
  expect_equal(length(p), 11)

  # p should have five layers at this point
  # because HELIX, STRAND and TURN added separately
  expect_equal(length(p$layers), 5)
  # https://stackoverflow.com/questions/13457562/how-to-determine-the-geom-type-of-each-layer-of-a-ggplot2-object/43982598#43982598
  # types of layers...
  expect_equal(class(p$layers[[1]]$geom)[1], "GeomRect")
  expect_equal(class(p$layers[[2]]$geom)[1], "GeomText")
  expect_equal(class(p$layers[[3]]$geom)[1], "GeomRect")
  expect_equal(class(p$layers[[4]]$geom)[1], "GeomRect")
  expect_equal(class(p$layers[[5]]$geom)[1], "GeomRect")

  # in this case nrow of layer 3 should be 4
  # and nrow of layer 4 should be 2
  expect_equal(nrow(p$layers[[3]]$data), 34) # 34 STRAND regions
  expect_equal(nrow(p$layers[[4]]$data), 10) # 10 HELIX regions
  expect_equal(nrow(p$layers[[5]]$data), 1) # 1 TURN

})
