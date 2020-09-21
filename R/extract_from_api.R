#' Create a dataframe of protein features from JSON object
#'
#' This function works on the object returned by the get_features() function.
#' It creates a data.frame of features and includes the accession number AND
#' an order number. It uses the extract_feat_acc function below.
#'
#' @param features_in_lists_of_six A list of lists returned by get_features()
#' The number of lists corresponds to the number of accession numbers queried
#' using get_features. The list of 6 contains protein names and features.
#'
#' @return A dataframe with 9 variables including type, description, begin,
#' end, length, accession, entryName, taxid and order for plotting.
#'
#' @examples
#' data("rel_json")
#' rel_data <- feature_to_dataframe(rel_json)
#' head(rel_data)
#'
#' data("five_rel_list")
#' prot_data <- feature_to_dataframe(five_rel_list)
#' head(prot_data)
#'
#' @export
# this function works on the object returned from multiple GET Uniprot API
# it creates a data.frame of features
# includes the accession number AND an order number
# it uses the extract_feat_acc function below
feature_to_dataframe <- function(features_in_lists_of_six){
    ################
    # loop to work through the API object and convert to data.frame
    # probably there is a better way to do this
    features_total_plot <- NULL
    for(i in 1:length(features_in_lists_of_six)){
        # the extract_feat_acc() function takes features into a data.frame
        features_temp <-
            drawProteins::extract_feat_acc(features_in_lists_of_six[[i]])
        features_temp$order <- i  # this order is needed for plotting later
        features_total_plot <- rbind(features_total_plot, features_temp)
    }
    return(features_total_plot)
}





#' Create a dataframe of protein features from JSON object
#'
#' Reduces data.frame of features to just phosphorylation sites. Uses a
#' subsetting step and a grep with the pattern "Phospho".
#'
#' @param features A dataframe of protein features, for example created by
#' the feature_to_dataframe() function.
#'
#' @return A dataframe that only contains protein phosphorylation sites from
#' Uniprot
#'
#' @examples
#' data("five_rel_data")
#' sites <- phospho_site_info(five_rel_data)
#' head(sites)
#'
#'
#' @export
phospho_site_info <- function(features){
    features <- features[features$type == "MOD_RES",]
    phospho_list <- grep("Phospho", features$description)
    phospho_features <- features[phospho_list,]
    return(phospho_features)
}


#' Create a dataframe of protein features from JSON object (List of 6)
#'
#' Converts the list of 6 JSON object created by getting the features from
#' UniProt. Used in the feature_to_dataframe(). Does not give order. Does not
#' operate on List of lists - just the list of 6.
#'
#' @param features_list A JSON object - list of 6 with features inside. Created
#' as one of the lists in the list of lists by the get_features() function.
#'
#' @return A dataframe with features: "type", "description", "begin", "end" and
#' adds accession, entryName and taxid for each row.
#'
#' @examples
#' data("five_rel_list")
#' one_protein_features <- extract_feat_acc(five_rel_list[[1]])
#' head(one_protein_features)
#'
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
    features_dataframe$length <-
        features_dataframe$end - features_dataframe$begin

    # Add 1 chain if missing
    if(!any(features_dataframe$type=="CHAIN")){
        features_dataframe <- rbind(features_dataframe,
                    data.frame(type = "CHAIN",
                    description = "NONE",
                    begin = 1,
                    end = nchar(features_list$sequence),
                    length = nchar(features_list$sequence) - 1))
    }

    # add accession number to each row of dataframe
    features_dataframe$accession <- rep(features_list$accession,
        times = nrow(features_dataframe))

    # add entryName (e.g. p65_HUMAN) to each row of dataframe
    features_dataframe$entryName <- rep(features_list$entryName,
        times = nrow(features_dataframe))

    # add taxid to each row of datafame
    features_dataframe$taxid <- rep(features_list$taxid,
        times = nrow(features_dataframe))

    return(features_dataframe)
}


#' Extract protein names into a list
#'
#' Extracts protein names from JSON object produced by a search of Uniprot
#' with a single protein asking for all the information.
#' The search produces a Large list that  contains all the Uniprot information
#' about a protein.
#'
#' @param protein_json A JSON object from a search with 14 primary parts
#'
#' @return A List of 6 with "accession", "name",
#' "protein.recommendedName.fullName",
#' gene.name.primary, gene.name.synonym and organism.name.scientific
#'
#' @examples
#' # using internal data
#' data("protein_json")
#' prot_names <- extract_names(protein_json)
#' # generates a list of 6
#'
#' \dontrun{
#' # access the Uniprot Protein API
#' uniprot_acc <- c("Q04206")  # change this for your fav protein
#' # Get UniProt entry by accession
#' acc_uniprot_url <-
#'     c("https://www.ebi.ac.uk/proteins/api/proteins?accession=")
#' comb_acc_api <- paste0(acc_uniprot_url, uniprot_acc)
#' # basic function is GET() which accesses the API
#' # requires internet access
#' protein <- httr::GET(comb_acc_api, accept_json())
#' status_code(protein)  # returns a 200 means it worked
#' # use content() function from httr to give us a list
#' protein_json <- httr::content(protein) # gives a Large list
#' # with 14 primary parts and lots of bits inside
#' # function from my package to extract names of protein
#' names <- extract_names(protein_json)
#' }
#'
#' @export
# function to extract names into a list
# from a JSON object
# JSON object created by getting Uniprot API output
# and using httr::content() function to turn in to JSON
extract_names <- function(protein_json){
    # Steph says create a variable of protein_json[[1]] to prevent repitition
    prot_info <- protein_json[[1]]
    # extract list of names...
    names <- list(
        accession = prot_info$accession,
        name = prot_info$id,
        protein.recommendedName.fullName =
            prot_info$protein$recommendedName$fullName$value,
        gene.name.primary = prot_info$gene[[1]]$name$value,
        gene.name.synonym = prot_info$gene[[1]]$synonyms[[1]]$value,
        organism.name.scientific = prot_info$organism$names[[1]]$value
    )
    return(names)
}
