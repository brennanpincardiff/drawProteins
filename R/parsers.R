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
#'
#'
parse_gff <- function(file_or_link){

  # key is to skip 2 rows and remove colomn names
  gff_data <- readr::read_tsv(file_or_link, skip = 2, col_names = FALSE)
  ##

  ## transform data so that I can use it to draw...
  colnames(gff_data) <- c("accession", "source", "type", "begin", "end",
    "X6", "X7", "X8", "X9", "X10")

  # add order
  gff_data$order <- 1
  # add entryName
  gff_data$entryName <- gff_data$accession
  # make capital
  gff_data$type <- toupper(gff_data$type)

  # OK, so description is none so need to use type...
  # add an if statement.. if there is a description use that but if not...
  # use type...

  # easiest thing to do is just to see if description is there...
  temp_text <- stringr::str_c(colnames(gff_data), collapse = "")
  temp_case <- stringr::str_detect(temp_text, "description")
  if(temp_case == FALSE){
    gff_data$description <- gff_data$type
  }
  return(gff_data)
}
