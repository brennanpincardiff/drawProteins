# source("http://www.bioconductor.org/biocLite.R ")
# biocLite()
# biocLite("biomaRt")
library(biomaRt)

# steps
# 1 choose a Mart with useMart()
# 2 choose a dataset with useMart()

# make query with getBM()
# with
# A. filter  -  restriction on the query
# e.g. Interpro ID(s) [e.g. IPR000001]
# e.g. chromosome_name
# B. attributes  -  values we are interested in to retrieve.
# C. values - values you want to

listMarts()
# from https://bioconductor.org/packages/release/bioc/vignettes/biomaRt/inst/doc/biomaRt.html

# chosing a database = MART and a dataset - gets more focussed each step...
ensembl = useMart("ensembl",
                  dataset="hsapiens_gene_ensembl")

# Retrieve all entrezgene identifiers and HUGO gene symbols of genes which have
# a “MAP kinase activity” GO term associated with it.

getBM(attributes = c('entrezgene','hgnc_symbol'),
      filters = 'go',
      values = 'GO:0004707',
      mart = ensembl)

# this is 14 proteins....

# create output in a dataframe and add uniprotswissprot
output <- getBM(attributes = c('unigene',
                               'uniprotswissprot',
                               'hgnc_symbol'),
                filters = 'go',
                values = 'GO:0004707',
                mart = ensembl)


# returns a dataframe... pull out uniprotIDs for drawing...
uniprotIDs <- output$uniprotswissprot
uniprotIDs[uniprotIDs==""] <- NA
uniprotIDs <- na.omit(uniprotIDs)
uniprotIDs <- as.character(uniprotIDs)
uniprotIDs <- unique(uniprotIDs)
uniprotIDs <- paste(uniprotIDs, collapse = " ")
# this can now be used in drawProteins

# now get features from Uniprot
library(magrittr)
library(drawProteins)
library(ggplot2)

uniprotIDs %>%
  drawProteins::get_features() %>%
  drawProteins::feature_to_dataframe() ->
  prot_data
# data frame with 722 observations

prot_data %>%
  geom_chains() %>%
  geom_domains -> p


p %>% geom_region  #
p %>% geom_repeat  #
p %>% geom_motif   #
p %>% geom_phospho()
# so best diagram like this...

prot_data %>%
  geom_chains() %>%
  geom_domains %>%
  geom_repeat %>%
  geom_motif %>%
  geom_phospho(size = 5) -> p


# add titles
p <- p + labs(x = "Amino acid number",         # label x-axis
              y = "",  # label y-axis
              title = "Schematic of human MapK proteins",
              subtitle = "circles = phosphorylation sites\nsource:Uniprot")
p

# white background and remove y-axis
p <- p + theme_bw() +  # white background
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank()) +
  theme(axis.ticks = element_blank(), axis.text.y = element_blank()) +
  theme(panel.border = element_blank())
p

# AND it works!!

# when we order these... can we include an alignment?
# we might be able to put these side by side....

