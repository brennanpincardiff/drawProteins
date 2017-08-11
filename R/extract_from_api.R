#' @export
# my first two functions for the package drawProteins

# function to extract names into a list
# from a JSON object
# JSON object created by getting Uniprot API output
# and using httr::content() function to turn in to JSON
extract_names <- function(protein_json){
  # Steph says create a variable of protein_json[[1]] to prevent repeating this below!!
  prot_info <- protein_json[[1]]
  # extract list of names...
  names <- list(
    accession = prot_info$accession,
    name = prot_info$id,
    protein.recommendedName.fullName = prot_info$protein$recommendedName$fullName$value,
    gene.name.primary = prot_info$gene[[1]]$name$value,
    gene.name.synonym = prot_info$gene[[1]]$synonyms[[1]]$value,
    organism.name.scientific = prot_info$organism$names[[1]]$value
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
# features_list <- prot_feat_list$features  # should be list..

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



#' @export
# I want to make the function work with the original JSON object
# rather than needing to manipulate outside of the function
# this seems to work.
# also data.frame now has accession number and entryName for each row

extractFeaturesListwithAcc <- function(prot_feat){

  # extract the list that we need for the object obtained from API
  prot_feat %>%
    httr::content() %>%    # this produces a List of 1 with a List of 6 inside
    purrr::flatten() -> features_list  # now just the List of 6 from inside

  # create the data.frame object called features
  features <- NULL
  for(i in 1:length(features_list$features)){
    featuresTemp <- c(features_list$features[[i]]$type,
                      as.character(features_list$features[[i]]$description),
                      as.numeric(features_list$features[[i]]$begin),
                      as.numeric(features_list$features[[i]]$end))
    features <- rbind(features, featuresTemp) # combine
  }

  features_dataframe <- as.data.frame(features, stringsAsFactors = FALSE)
  colnames(features_dataframe) <- c("type", "description", "begin", "end")
  features_dataframe$begin <- as.numeric(features_dataframe$begin)
  features_dataframe$end <- as.numeric(features_dataframe$end)
  features_dataframe$length <- features_dataframe$end - features_dataframe$begin
  features_dataframe$accession <- rep(features_list$accession, times = nrow(features_dataframe))
  features_dataframe$entryName <- rep(features_list$entryName, times = nrow(features_dataframe))
  return(features_dataframe)
}


#' @export
# this function works on a List of 6 from the Uniprot API
# it creates a data.frame of features
# and now includes the accession number
# it should be a better function to use than either of the above functions
# which I will probably remove at some point
extract_feat_acc <- function(features_list){

  # create the data.frame object called features
  features <- NULL
  for(i in 1:length(features_list$features)){
    if(is.null(features_list$features[[i]]$description) == TRUE){
      featuresTemp <- c(features_list$features[[i]]$type,
                        "NONE",
                        as.numeric(features_list$features[[i]]$begin),
                        as.numeric(features_list$features[[i]]$end))
    } else{
      featuresTemp <- c(features_list$features[[i]]$type,
                        as.character(features_list$features[[i]]$description),
                        as.numeric(features_list$features[[i]]$begin),
                        as.numeric(features_list$features[[i]]$end))
    }
    features <- rbind(features, featuresTemp) # combine
  }

  features_dataframe <- as.data.frame(features, stringsAsFactors = FALSE)
  colnames(features_dataframe) <- c("type", "description", "begin", "end")
  features_dataframe$begin <- as.numeric(features_dataframe$begin)
  features_dataframe$end <- as.numeric(features_dataframe$end)
  features_dataframe$length <- features_dataframe$end - features_dataframe$begin
  features_dataframe$accession <- rep(features_list$accession, times = nrow(features_dataframe))
  features_dataframe$entryName <- rep(features_list$entryName, times = nrow(features_dataframe))
  return(features_dataframe)
}

