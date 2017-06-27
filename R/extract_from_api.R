#' @export
# my first two functions for the package drawProteins

# function to extract names into a list
# from a JSON object 
# JSON object created by getting Uniprot API output
# and using httr::content() function to turn in to JSON 
extract_names <- function(protein_json){
  # extract list of names...
  names <- list(
    accession = protein_json[[1]]$accession,
    name = protein_json[[1]]$id,
    protein.recommendedName.fullName = protein_json[[1]]$protein$recommendedName$fullName$value,
    gene.name.primary = protein_json[[1]]$gene[[1]]$name$value,
    gene.name.synonym = protein_json[[1]]$gene[[1]]$synonyms[[1]]$value,
    organism.name.scientific = protein_json[[1]]$organism$names[[1]]$value
  )
  return(names)
}



#' @export
# function to extract features from list into a dataframe
# from a JSON object 
# JSON object created by getting Uniprot Features API output
# using following code
# prot_feat %>%
#  content() %>%
#  flatten() -> prot_feat_list  # list of 6
#
# features_list <- prot_feat_list$features  # should be list...


extractFeaturesList <- function(features_list){
  features <- NULL
  for(i in 1:length(features_list)){
    featuresTemp <- c(features_list[[i]]$type,
                      as.character(features_list[[i]]$description),
                      as.numeric(features_list[[i]]$begin),
                      as.numeric(features_list[[i]]$end))
    features <- rbind(features, featuresTemp) # combine
  }
  
  features_dataframe <- as.data.frame(features, stringsAsFactors = FALSE)
  colnames(features_dataframe) <- c("type", "description", "begin", "end")
  features_dataframe$begin <- as.numeric(features_dataframe$begin)
  features_dataframe$end <- as.numeric(features_dataframe$end)
  features_dataframe$length <- features_dataframe$end - features_dataframe$begin
  return(features_dataframe)
}



#' @export
# function to reduce data.frame of features to just phosphorylation sites
phospho_site_info <- function(features){
  features <- features[features$type == "MOD_RES",]
  phospho_list <- grep("Phospho", features$description)
  phospho_features <- features[phospho_list,]
  return(phospho_features)
}


  
  