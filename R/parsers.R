### parse_gff
#' Reformat file or url in gff format to allow to draw
#'
#' \code{parse_gff} loads a file or downloads from an url if provided
#' protein information that is then changed to allow it to work with
#' \code{draw_canvas} and other draw functions in drawProteins.
#'
#' @param file_or_link link in gff format or a file in gff format that can be
#' read by \code{read_tsv} function from the readr package.
#'
#' @return Dataframe of one or more rows with the following column
#' names: 'accession', 'source', 'type', 'begin', 'end', 'order',
#' 'entryName', 'description'. Must contain a minimum of one "CHAIN" as
#' data$type to allow plotting.
#'
#' @export
#'
#' @examples
#' data <- parse_gff("https://www.uniprot.org/uniprot/Q04206.gff")
#'
parse_gff <- function(file_or_link){
    begin <- attribute <- type <- NULL
    # key is to ignore comment rows and remove colomn names
    gff_data <- readr::read_tsv(file_or_link, comment = "##", col_names = FALSE)

    ## format should only have 9 columns so remove column 10
    if(ncol(gff_data)>9){
      gff_data <- gff_data[,1:9]}

    ## add column names data so that I can use it to draw...
    colnames(gff_data) <- c("accession", "source", "type", "begin", "end",
    "score", "strand", "frame", "attribute")

    # remove rows that don't contain a begin
    gff_data <- dplyr::filter(gff_data, begin > 0)

    # create description file
    gff_data <- tidyr::separate(gff_data,
      attribute, into = c("description", "second"), sep = ";",
      extra = "merge", fill = "right")

    gff_data$description <- gsub("Note=", "", gff_data$description)

    # add order
    gff_data$order <- 1
    # make capital
    gff_data$type <- toupper(gff_data$type)
    # add entryName
    gff_chain <- dplyr::filter(gff_data, type == "CHAIN")
    gff_data$entryName <- gsub("Note=", "", as.character(gff_chain[1,10]))
    return(gff_data)
}
