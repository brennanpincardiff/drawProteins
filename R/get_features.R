#' @export
# new function
# called get_features
get_features <- function(proteins_acc){

  # accession numbers need to be combined with "%2C" for Uniprot API
  proteins_acc_url <- gsub(" ", "%2C", proteins_acc)

  # this is the baseurl for a multiple features query
  baseurl <- "https://www.ebi.ac.uk/proteins/api/features?offset=0&size=100&accession="

  # make url to GET features for multiple accession numbers
  url <- paste0(baseurl, proteins_acc_url)

  # basic function is GET() which accesses the API
  # accept_json() argument gives a JSON object
  prots_feat <- httr::GET(url,
                    accept_json())

  httr::status_code(prots_feat)  # returns a 200 - good

# extract content() - gives a list with length = number of acc no
   prots_feat_red <- httr::content(prots_feat)
   return(prots_feat_red)
}
