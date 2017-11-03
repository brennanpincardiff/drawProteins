### geom_chains
#' Create ggplot object with protein chains from feature database
#'
#' \code{geom_chains} uses the dataframe containing the protein features to
#' plot the chains, the full length proteins. It creates the basic plot element
#' by determining the length of the longest protein. The ggplot function
#' \code{\link[ggplot2]{geom_rect}} is then used to draw each of the protein
#' chains proportional to their number of amino acids (length).
#'
#' @param data Dataframe of one or more rows with the following column
#' names: 'type', 'description', 'begin', 'end', 'length', 'accession',
#' 'entryName', 'taxid', 'order'. Must contain a minimum of one "CHAIN" as
#' data$type.
#' @param outline Colour of the outline of each chain.
#' @param fill Colour of the fill of each chain.
#' @param label_chains Option to label chains or not.
#' @param labels Vector with source of names for the chains. EntryName used as
#' default but can be changed.
#' @param size Size of the outline of the chains.
#' @param label_size Size of the text used for labels.
#'
#' @return A ggplot object either in the plot window or as an object.
#'
#' @examples
#' # draws five chains corresponding to human NF-kappaB proteins
#' geom_chains(five_rel_data)
#'
#' # draws five chains with different colours to default
#' five_rel_data %>%
#' geom_chains(label_chains = FALSE,
#'     fill = "red",
#'     outline = "grey")
#'
#' # combines well with geom_domains to plot chains and domains.
#' p <- geom_chains(five_rel_data, label_size = 1.25)
#' p <- geom_region(p, five_rel_data)
#' p
#'
#' @export
geom_chains <- function(data = data,
                        outline = "black",
                        fill = "grey",
                        label_chains = TRUE,
                        labels = data[data$type == "CHAIN",]$entryName,
                        size = 0.5,
                        label_size = 4){
    begin=end=NULL
    p <-ggplot2::ggplot() +
    ggplot2::ylim(0.5, max(data$order)+0.5) +
    ggplot2::xlim(-max(data$end)*0.2,
                    max(data$end) + max(data$end)*0.1) +
    ggplot2::geom_rect(data = data[data$type == "CHAIN",],
                        mapping=ggplot2::aes(xmin=begin,
                                            xmax=end,
                                            ymin=order-0.2,
                                            ymax=order+0.2),
                        colour = outline,
                        fill = fill,
                        size = size)

    if(label_chains == TRUE){
        p <- p +
            ggplot2::annotate("text", x = -10,
                y = data[data$type == "CHAIN",]$order,
                        label = labels,
                        hjust = 1,
                        size = label_size)
    }
    return(p)
}


### geom_domains
#' Add protein domains to ggplot object.
#'
#' \code{geom_domains} adds domains to the ggplot object created by
#' \code{\link{geom_chains}}.
#' It uses the data object.
#' The ggplot function
#' \code{\link[ggplot2]{geom_rect}} is used to draw each of the domain
#' chains proportional to their number of amino acids (length).
#'
#' @param p ggplot object ideally created with \code{\link{geom_chains}}.
#' @param data Dataframe of one or more rows with the following column
#' names: 'type', 'description', 'begin', 'end', 'length', 'accession',
#' 'entryName', 'taxid', 'order'. Must contain a minimum of one "CHAIN" as
#' data$type.
#' @param label_domains Option to label domains or not.
#' @param label_size Size of the text used for labels.
#' @return A ggplot object either in the plot window or as an object with an
#' additional geom_rect layer.
#'
#' @examples
#' p <- geom_chains(five_rel_data, label_size = 1.25)
#' p <- geom_domains(p, five_rel_data)
#' p
#'
#' @export
# called geom_domains to plot just the domains
geom_domains <- function(p,
                        data = data,
                        label_domains = TRUE,
                        label_size = 4){
    begin=end=description=NULL
    p <- p + ggplot2::geom_rect(data= data[data$type == "DOMAIN",],
            mapping=ggplot2::aes(xmin=begin,
                        xmax=end,
                        ymin=order-0.25,
                        ymax=order+0.25,
                        fill=description))

    if(label_domains == TRUE){
        p <- p + ggplot2::geom_label(data = data[data$type == "DOMAIN", ],
                        ggplot2::aes(x = begin + (end-begin)/2,
                            y = order,
                            label = description),
                            size = label_size)
    }

    return(p)
}



### geom_phospho
#' Add protein phosphorylation sites to ggplot object.
#'
#' \code{geom_phospho} adds phosphorylation sites to ggplot object created by
#' \code{\link{geom_chains}}.
#' It uses the data object.
#' The ggplot function
#' \code{\link[ggplot2]{geom_point}} is used to draw each of the
#' phosphorylation sites at their location as determined by data object.
#'
#' @param p ggplot object ideally created with \code{\link{geom_chains}}.
#' @param data Dataframe of one or more rows with the following column
#' names: 'type', 'description', 'begin', 'end', 'length', 'accession',
#' 'entryName', 'taxid', 'order'. Must contain a minimum of one "CHAIN" as
#' data$type.
#' @param size Size of the circle
#' @param fill Colour of the circle.
#'
#' @return A ggplot object either in the plot window or as an object with an
#' additional geom_point layer.
#'
#' @examples
#' # combines will with geom_domains to plot chains and phosphorylation sites.
#' library(magrittr)
#' p <- geom_chains(five_rel_data, label_size = 1.25)
#' p <- geom_phospho(p, five_rel_data)
#' p
#'
#' @export
# called geom_phospho
# to draw phosphorylation sites on the protein with geom_point()
geom_phospho <- function(p, data = data,
                        size = 2,
                        fill = "yellow"){
    begin=end=description=NULL
    p <- p + ggplot2::geom_point(data = drawProteins::phospho_site_info(data),
                                ggplot2::aes(x = begin,
                        y = order+0.25),
                        shape = 21,
                        colour = "black",
                        fill = fill,
                        size = size)
    return(p)
}



### geom_region
#' Add protein region sites to ggplot object.
#'
#' \code{geom_region} adds protein regions from Uniprot to ggplot object
#' created by \code{\link{geom_chains}}.
#' It uses the data object.
#' The ggplot function
#' \code{\link[ggplot2]{geom_rect}} is used to draw each of the
#' regions proportional to their number of amino acids (length).
#'
#' @param p ggplot object ideally created with \code{\link{geom_chains}}.
#' @param data Dataframe of one or more rows with the following column
#' names: 'type', 'description', 'begin', 'end', 'length', 'accession',
#' 'entryName', 'taxid', 'order'. Must contain a minimum of one "CHAIN" as
#' data$type.
#' @return A ggplot object either in the plot window or as an object with an
#' additional geom_rect layer.
#'
#' @examples
#' # combines well with geom_domains to plot chains and regions.
#' p <- geom_chains(five_rel_data, label_size = 1.25)
#' p <- geom_region(p, five_rel_data)
#' p
#'
#' @export
# called geom_region
# to draw REGIONs
geom_region <- function(p, data = data){
    begin=end=description=NULL
    ## plot motifs fill by description
    p <- p + ggplot2::geom_rect(data= data[data$type == "REGION",],
                        mapping=ggplot2::aes(xmin=begin,
                                xmax=end,
                                ymin=order-0.25,
                                ymax=order+0.25,
                                fill=description))

    return(p)
}







### geom_motif
#' Add protein motifs sites to ggplot object.
#'
#' \code{geom_motif} adds protein motifs from Uniprot to ggplot object created
#' by \code{\link{geom_chains}}.
#' It uses the data object.
#' The ggplot function
#' \code{\link[ggplot2]{geom_rect}} is used to draw each of the
#' motifs proportional to their number of amino acids (length).
#'
#' @param p ggplot object ideally created with \code{\link{geom_chains}}.
#' @param data Dataframe of one or more rows with the following column
#' names: 'type', 'description', 'begin', 'end', 'length', 'accession',
#' 'entryName', 'taxid', 'order'. Must contain a minimum of one "CHAIN" as
#' data$type.
#' @return A ggplot object either in the plot window or as an object with an
#' additional geom_rect layer.
#'
#' @examples
#' # combines will with geom_domains to plot chains and protein motifs.
#' p <- geom_chains(five_rel_data, label_size = 1.25)
#' p <- geom_motif(p, five_rel_data)
#' p
#'
#' @export
# called geom_motif
# to draw MOTIFs - no label at the moment.
geom_motif <- function(p, data = data){
    begin=end=description=NULL
    ## plot motifs fill by description
    p <- p + ggplot2::geom_rect(data= data[data$type == "MOTIF",],
                                mapping=ggplot2::aes(xmin=begin,
                                xmax=end,
                                ymin=order-0.25,
                                ymax=order+0.25,
                                fill=description))

    return(p)
}


### geom_repeat
#' Add protein repeats sites to ggplot object.
#'
#' \code{geom_repeat} adds protein repeats from Uniprot to ggplot object
#' created by \code{\link{geom_chains}}. It uses the data object.
#' The ggplot function \code{\link[ggplot2]{geom_rect}}
#' is used to draw each of the motifs proportional to their number of
#' amino acids (length).
#'
#' @param p ggplot object ideally created with \code{\link{geom_chains}}.
#' @param data Dataframe of one or more rows with the following column
#' names: 'type', 'description', 'begin', 'end', 'length', 'accession',
#' 'entryName', 'taxid', 'order'. Must contain a minimum of one "CHAIN" as
#' data$type.
#' @param label_size Size of text used for labels of protein repeats.
#' @param outline Colour of the outline of each repeat.
#' @param fill Colour of the fill of each repeat.
#' @param label_repeats Option to label repeats or not.
#' @return A ggplot object either in the plot window or as an object with an
#' additional geom_rect layer.
#'
#' @examples
#' # combines will with geom_domains to plot chains and protein repeat.
#' p <- geom_chains(five_rel_data, label_size = 1.25)
#' p <- geom_repeat(p, five_rel_data)
#' p

#'
#' @export
# called geom_repeat
# to draw REPEATs & label
geom_repeat <- function(p, data = data,
                        label_size = 2,
                        outline = "dimgrey",
                        fill = "dimgrey",
                        label_repeats = TRUE){
    begin=end=description=NULL
    ## step 6 plot repeats fill by description
    p <- p + ggplot2::geom_rect(data= data[data$type == "REPEAT",],
                        mapping=ggplot2::aes(xmin=begin,
                                xmax=end,
                                ymin=order-0.25,
                                ymax=order+0.25),
                        colour = outline,
                        fill = fill)

    if(label_repeats == TRUE){
        # label repeats (for this they are ANK but remove digits)
        p <- p + ggplot2::geom_text(data = data[data$type == "REPEAT",],
                                ggplot2::aes(x = begin + (end-begin)/2,
                                y = order,
                                label = gsub("\\d", "", description)),
                                size = label_size)
    }
    return(p)
}
