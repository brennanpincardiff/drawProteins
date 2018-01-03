#' Create a new dataframe of protein features from dataframe with multiple
#' transcripts separated so that each transcript is drawn separtely with
#' only the appropriate features.
#'
#' This function works on the object returned by the get_features() function.
#' It creates a data.frame of features and includes the accession number AND
#' an order number. It uses the extract_feat_acc function
#'
#' @param data Dataframe of one or more rows with the following column
#' names: 'type', 'description', 'begin', 'end', 'length', 'accession',
#' 'entryName', 'taxid', 'order'. Must contain a minimum of one "CHAIN" as
#' data$type.
#'
#' @return A dataframe with extra rows if there were multiple transcripts
#' present. Extra transcripts will have an order at the end of the object
#' Each new row should have 9 variables including type, description, begin,
#' end, length, accession, entryName, taxid and order for plotting.
#'
#' @examples
#' data(five_rel_data)
#' new_data <- extract_transcripts(five_rel_data)
#' # because there are two entries with two transcripts
#' max(new_data$order) # should now be 7...
#'
#' @export
# multiple protein transcripts seems like an important challenge
# challenge we have multiple "chains" on same order...
# create a function to pull out multiple transcripts
# if there are transcripts i.e. more than one chain per order
# need to increase order...
# need to remove features that are not in the protein
extract_transcripts <- function(data){
    max_order <- max(data$order)
    transcript_data <- NULL
    new_order <- max(data$order)
    for(i in 1:max_order) {
        # if number of chains > 1 then adjust order...
        data_s <- dplyr::filter(data, order == i)
        data_s_c <- dplyr::filter(data, order == i & data$type == "CHAIN")
        if (nrow(data_s_c) >1){   # means there is more than one chain
            # print("true")
            extra_chains <- nrow(data_s_c)-1
            for(j in 1:extra_chains){
                # print("loop")
                # print(j)
                # duplicate the chain and then remove domains
                new_order <- new_order + 1
                data_s$order <- new_order # so that gives us a new order
                # remove first chain (first row)
                data_s <- data_s[2:nrow(data_s),]
                # remove domains that are longer than this transcript
                data_s <- dplyr::filter(data_s, data_s$begin < data_s[1,4])
                # because transcripts may not start at 1
                # keep them if the domains before the start
                data_s <- dplyr::filter(data_s, data_s$begin >= data_s[1,3])
                transcript_data <- rbind(transcript_data, data_s)
            }
        }
    }
    new_data <- rbind(data, transcript_data)
    return(new_data)
}

