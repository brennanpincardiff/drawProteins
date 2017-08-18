# this is my test script - so I need to divide it into tests really

# N.B. need to assess coverage using the coverage package, I think.
# rel schematic again
library(drawProteins)
library(magrittr)
library(devtools)
# accession numbers of five rel proteins
"Q04206 Q01201 Q04864 P19838 Q00653" %>%
  drawProteins::get_features() %>%
  drawProteins::feature_to_dataframe() ->
  prot_data
# N.B. should I have to library(httr)? should it not be libraried already?
# N.B. prot_data object now has 9 variables - so testing will fail!

prot_data %>%
  geom_chains() %>%
  geom_domains -> p


p %>% geom_region  # adds interaction domains, leucine zipper et al
p %>% geom_repeat  # adds Ank repeats
p %>% geom_motif   # adds 9aa Transactivation domain & NLS

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
              title = "Schematic of human NFkappaB proteins",
              subtitle = "circles = phosphorylation sites\nRHD = Rel Homology Domain\nsource:Uniprot")
p

# white background and remove y-axis
p <- p + theme_bw() +  # white background
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank()) +
  theme(axis.ticks = element_blank(), axis.text.y = element_blank()) +
  theme(panel.border = element_blank())
p
