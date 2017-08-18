#' @export
# get_features function to GET protein data from Uniprot
get_features <- function(proteins_acc){

  # accession numbers need to be combined with "%2C" for Uniprot API
  proteins_acc_url <- gsub(" ", "%2C", proteins_acc)

  # this is the baseurl for a multiple features query
  baseurl <- "https://www.ebi.ac.uk/proteins/api/features?offset=0&size=100&accession="

  # make url to GET features for multiple accession numbers
  url <- paste0(baseurl, proteins_acc_url)

  # basic function is GET() which accesses the API
  # accept_json() argument gives a JSON object
  prots_feat <- httr::GET(url, httr::accept_json())

  code <- httr::status_code(prots_feat)  # if it returns a 200 - that's good
  if(code == 200){
    print("Download has worked")
  } else {print(paste("An error has occured. Code:", code))}

# extract content() - gives a list with length = number of acc no
   prots_feat_red <- httr::content(prots_feat)
   return(prots_feat_red)
}


### error handling required:
# no internet connection
# one or more accession numbers not in uniprot - warning

### enhanced functionality for future:
# R object with accession numbers
# other separation options
# import file (csv, tsv, excel)
# import from other Bioconductor packages
# formats other than accession numbers?
# - other databases...

