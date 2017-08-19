#' Create ggplot object with protein chains from feature database
#'
#' \code{geom_chains} uses the dataframe containing the protein features to plot
#'  the chains, the full length proteins. It creates the basic plot element by
#'  determining the length of the longest protein. The ggplot function
#'  \code{\link[ggplot2]{geom_rect}} is then used to draw each of the protein
#'  chains proportional to their number of amino acids (length).
#'
#' @param prot_data Dataframe of one or more rows with the following column
#'  names: "type", "description", "begin", "end", "length", "accession",
#'  "entryName", "taxid", "order". Must contain a minimum of one "CHAIN" as
#'  prot_data$type.
#'
#' @param outline Colour of the outline of each chain.
#'
#' @param fill Colour of the fill of each chain.
#'
#' @param label_chain Option to label chains or not.
#'
#' @param labels Vector with source of names for the chains. EntryName used as
#' default but can be changed.
#'
#' @param size Size of the outline of the chains.
#'
#' @param label_size Size of the text used for labels.
#'
#' @return A ggplot object either in the plot window or as an object.
#'
#' @examples
#' # draws five chains corresponding to human NF-kappaB proteins
#' geom_chains(prot_data)
#'
#' # draws five chains with different colours to default
#' library(magrittr)
#' prot_data %>%
#' geom_chains(label_chains = FALSE,
#'            fill = "red",
#'            outline = "grey")
#'
#' # combines will with geom_domains to plot chains and domains.
#' library(magrittr)
#' prot_data %>%
#'      geom_chains(label_size = 1.25) %>%
#'      geom_domains(label_size = 1.25) -> p
#'      p
#'
#' @export
geom_chains <- function(prot_data = prot_data,
                        outline = "black",
                        fill = "grey",
                        label_chains = TRUE,
                        labels = prot_data[prot_data$type == "CHAIN",]$entryName,
                        size = 0.5,
                        label_size = 4){

    p <-ggplot2::ggplot() +
    ggplot2::ylim(0.5, max(prot_data$order)+0.5) +
    ggplot2::xlim(-max(prot_data$end)*0.2,
                  max(prot_data$end) + max(prot_data$end)*0.1) +
    ggplot2::geom_rect(data = prot_data[prot_data$type == "CHAIN",],
                       mapping=ggplot2::aes(xmin=begin,
                                   xmax=end,
                                   ymin=order-0.2,
                                   ymax=order+0.2),
                       colour = outline,
                       fill = fill,
                       size = size)

  if(label_chains == TRUE){
    p <- p +
      ggplot2::annotate("text", x = -10, y = prot_data[prot_data$type == "CHAIN",]$order,
                        label = labels,
                        hjust = 1,
                        size = label_size)
  }
  return(p)
}




#' Add domains to a ggplot object.
#'
#' \code{geom_domains} adds domains to the ggplot object created by
#' \code{\link{geom_chains}}.
#'  It uses the prot_data object.
#'   The ggplot function
#'  \code{\link[ggplot2]{geom_rect}} is used to draw each of the domain
#'  chains proportional to their number of amino acids (length).
#'
#' @param p ggplot object ideally created with \code{\link{geom_chains}}.
#'
#' @param label_domains Option to label domains or not.
#'
#' @param label_size Size of the text used for labels.
#'
#' @return A ggplot object either in the plot window or as an object with an
#' additional geom_rect layer.
#'
#' @examples
#' # combines will with geom_domains to plot chains and domains.
#' library(magrittr)
#' prot_data %>%
#'      geom_chains(label_size = 1.25) %>%
#'      geom_domains(label_size = 1.25) -> p
#'      p
#'
#' @export
# called geom_domains to plot just the domains
geom_domains <- function(p,
                         label_domains = TRUE,
                         label_size = 4){
  p <- p + ggplot2::geom_rect(data= prot_data[prot_data$type == "DOMAIN",],
            mapping=ggplot2::aes(xmin=begin,
                        xmax=end,
                        ymin=order-0.25,
                        ymax=order+0.25,
                        fill=description))

  if(label_domains == TRUE){
    p <- p + ggplot2::geom_label(data = prot_data[prot_data$type == "DOMAIN", ],
                        ggplot2::aes(x = begin + (end-begin)/2,
                 y = order,
                 label = description),
             size = label_size)
  }

  return(p)
}

#' @export
# called geom_phospho
# to draw phosphorylation sites on the protein with geom_point()
geom_phospho <- function(p,
                         size = 2,
                         fill = "yellow"){
  p <- p + ggplot2::geom_point(data = drawProteins::phospho_site_info(prot_data),
                               ggplot2::aes(x = begin,
                          y = order+0.25),
                      shape = 21,
                      colour = "black",
                      fill = fill,
                      size = size)
  return(p)
}

#' @export
# called geom_motif
# to draw MOTIFs - no label at the moment.
geom_region <- function(p){
  ## plot motifs fill by description
  p <- p + ggplot2::geom_rect(data= prot_data[prot_data$type == "REGION",],
                     mapping=ggplot2::aes(xmin=begin,
                                 xmax=end,
                                 ymin=order-0.25,
                                 ymax=order+0.25,
                                 fill=description))

  return(p)
}


#' @export
# called geom_motif
# to draw MOTIFs - no label at the moment.
geom_motif <- function(p){
  ## plot motifs fill by description
  p <- p + ggplot2::geom_rect(data= prot_data[prot_data$type == "MOTIF",],
                     mapping=ggplot2::aes(xmin=begin,
                                 xmax=end,
                                 ymin=order-0.25,
                                 ymax=order+0.25,
                                 fill=description))

  return(p)
}


#' @export
# called geom_repeat
# to draw REPEATs & label
geom_repeat <- function(p,
                        label_size = 2,
                        outline = "dimgrey",
                        fill = "dimgrey",
                        label_repeats = TRUE){
  ## step 6 plot repeats fill by description
  p <- p + ggplot2::geom_rect(data= prot_data[prot_data$type == "REPEAT",],
                     mapping=ggplot2::aes(xmin=begin,
                                 xmax=end,
                                 ymin=order-0.25,
                                 ymax=order+0.25),
                     colour = outline,
                     fill = fill)

  if(label_repeats == TRUE){
    # label repeats (for this they are ANK but remove digits)
    p <- p + ggplot2::geom_text(data = prot_data[prot_data$type == "REPEAT",],
                                ggplot2::aes(x = begin + (end-begin)/2,
                           y = order,
                           label = gsub("\\d", "", description)),
                       size = label_size)
    }
  return(p)
}
