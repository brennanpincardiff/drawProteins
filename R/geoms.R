### draw_canvas
#' Create ggplot2 object with protein chains from feature database
#'
#' \code{draw_canvas} uses the dataframe containing the protein features to
#' creates the basic plot element by determining the length of the longest
#' protein.
#'
#' @param data Dataframe of one or more rows with the following column
#' names: 'type', 'description', 'begin', 'end', 'length', 'accession',
#' 'entryName', 'taxid', 'order'. Must contain a minimum of one "CHAIN" as
#' data$type.
#'
#' @return A ggplot2 object either in the plot window or as an object.
#'
#' @examples
#' # draws a blank canvas of the correct size
#' data("five_rel_data")
#' draw_canvas(five_rel_data)
#'
#' # combines with draw_chains to plot and label chains.
#' data("five_rel_data")
#' p <- draw_canvas(five_rel_data)
#' p <- draw_chains(p, five_rel_data)
#' p
#'
#' @import ggplot2
#'
#' @export
draw_canvas <- function(data = data){
    begin=end=NULL
    p <- ggplot2::ggplot()
    p <- p + ggplot2::ylim(0.5, max(data$order)+0.5)
    p <- p + ggplot2::xlim(-max(data$end, na.rm=TRUE)*0.1,
                           max(data$end, na.rm=TRUE) + max(data$end, na.rm=TRUE)*0.1)
    p <- p + ggplot2::labs(x = "Amino acid number") # label x-axis
    p <- p + ggplot2::labs(y = "") # label y-axis

    return(p)
}


### draw_chains
#' Create ggplot2 object with protein chains from feature database
#'
#' \code{draw_chains} uses the dataframe containing the protein features to
#' plot the chains, the full length proteins. It creates the basic plot element
#' by determining the length of the longest protein. The ggplot2 function
#' \code{geom_rect} is then used to draw each of the protein
#' chains proportional to their number of amino acids (length).
#'
#' @param p ggplot2 object ideally created with \code{\link{draw_canvas}}.
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
#' @return A ggplot2 object either in the plot window or as an object.
#'
#' @examples
#' # combines with draw_canvas to plot and label chains.
#' data("five_rel_data")
#' p <- draw_canvas(five_rel_data)
#' p <- draw_chains(p, five_rel_data)
#' p
#'
#' # draws five chains with different colours to default
#' data("five_rel_data")
#' p <- draw_canvas(five_rel_data)
#' draw_chains(p, five_rel_data,
#'     label_chains = FALSE,
#'     fill = "red",
#'     outline = "grey")
#'
#' # combines with draw_chains to plot chains and domains.
#' data("five_rel_data")
#' p <- draw_canvas(five_rel_data)
#' p <- draw_chains(p, five_rel_data, label_size = 1.25)
#' p <- draw_regions(p, five_rel_data)
#' p
#'
#' @export
draw_chains <- function(p,
                        data = data,
                        outline = "black",
                        fill = "grey",
                        label_chains = TRUE,
                        labels = data[data$type == "CHAIN",]$entryName,
                        size = 0.5,
                        label_size = 4){
    begin=end=NULL
    p <- p + ggplot2::geom_rect(data = data[data$type == "CHAIN",],
                        mapping=ggplot2::aes(xmin=begin,
                                            xmax=end,
                                            ymin=order-0.2,
                                            ymax=order+0.2),
                        colour = outline,
                        fill = fill,
                        size = size)

    if(label_chains == TRUE){
        p <- p +
            scale_y_continuous(breaks = seq_along(unique(labels)),
                               labels = unique(labels))
    }
    return(p)
}


### draw_domains
#' Add protein domains to ggplot2 object.
#'
#' \code{draw_domains} adds domains to the ggplot2 object created by
#' \code{\link{draw_chains}}.
#' It uses the data object.
#' The ggplot2 function \code{geom_rect} is used to draw each of the domain
#' chains proportional to their number of amino acids (length).
#'
#' @param p ggplot2 object ideally created with \code{\link{draw_canvas}}.
#' @param data Dataframe of one or more rows with the following column
#' names: 'type', 'description', 'begin', 'end', 'length', 'accession',
#' 'entryName', 'taxid', 'order'. Must contain a minimum of one "CHAIN" as
#' data$type.
#' @param label_domains Option to label domains or not.
#' @param label_size Size of the text used for labels.
#' @param show.legend Option to include legend in this layer
#' @return A ggplot2 object either in the plot window or as an object with an
#' additional geom_rect layer.
#'
#' @examples
#' # combines with draw_chains to plot chains and domains.
#' data("five_rel_data")
#' p <- draw_canvas(five_rel_data)
#' p <- draw_chains(p, five_rel_data, label_size = 1.25)
#' p <- draw_domains(p, five_rel_data)
#' p
#'
#' @export
# called draw_domains to plot just the domains
draw_domains <- function(p,
                        data = data,
                        label_domains = TRUE,
                        label_size = 4,
                        show.legend = TRUE){
    begin=end=description=NULL
    p <- p + ggplot2::geom_rect(data= data[data$type == "DOMAIN",],
            mapping=ggplot2::aes(xmin=begin,
                        xmax=end,
                        ymin=order-0.25,
                        ymax=order+0.25,
                        fill=description),
                        show.legend = show.legend)

    if(label_domains == TRUE){
        p <- p + ggplot2::geom_label(data = data[data$type == "DOMAIN", ],
                        ggplot2::aes(x = begin + (end-begin)/2,
                            y = order,
                            label = description),
                            size = label_size)
    }

    return(p)
}



### draw_phospho
#' Add protein phosphorylation sites to ggplot2 object.
#'
#' \code{draw_phospho} adds phosphorylation sites to ggplot2 object created by
#' \code{\link{draw_canvas}} and \code{\link{draw_chains}}.
#' It uses the data object.
#' The ggplot2 function
#' \code{\link[ggplot2]{geom_point}} is used to draw each of the
#' phosphorylation sites at their location as determined by data object.
#'
#' @param p ggplot2 object ideally created with \code{\link{draw_canvas}}.
#' @param data Dataframe of one or more rows with the following column
#' names: 'type', 'description', 'begin', 'end', 'length', 'accession',
#' 'entryName', 'taxid', 'order'. Must contain a minimum of one "CHAIN" as
#' data$type.
#' @param size Size of the circle
#' @param fill Colour of the circle.
#' @param show.legend Option to include legend in this layer
#'
#' @return A ggplot2 object either in the plot window or as an object with an
#' additional geom_point layer.
#'
#' @examples
#' # combines will with draw_domains to plot chains and phosphorylation sites.
#' data("five_rel_data")
#' p <- draw_canvas(five_rel_data)
#' p <- draw_chains(p, five_rel_data, label_size = 1.25)
#' p <- draw_phospho(p, five_rel_data)
#' p
#'
#' @export
# called draw_phospho
# to draw phosphorylation sites on the protein with geom_point()
draw_phospho <- function(p, data = data,
                        size = 2,
                        fill = "yellow",
                        show.legend = FALSE){
    begin=end=description=NULL
    p <- p + ggplot2::geom_point(data = drawProteins::phospho_site_info(data),
                                ggplot2::aes(x = begin,
                        y = order+0.25),
                        shape = 21,
                        colour = "black",
                        fill = fill,
                        size = size,
                        show.legend = show.legend)
    return(p)
}



### draw_regions
#' Add protein region sites to ggplot2 object.
#'
#' \code{draw_regions} adds protein regions from Uniprot to ggplot2 object
#' created by \code{\link{draw_canvas}} \code{\link{draw_chains}}.
#' It uses the data object.
#' The ggplot2 function \code{geom_rect} is used to draw each of the
#' regions proportional to their number of amino acids (length).
#'
#' @param p ggplot2 object ideally created with \code{\link{draw_canvas}}.
#' @param data Dataframe of one or more rows with the following column
#' names: 'type', 'description', 'begin', 'end', 'length', 'accession',
#' 'entryName', 'taxid', 'order'. Must contain a minimum of one "CHAIN" as
#' data$type.
#' @param show.legend Option to include legend in this layer
#' @return A ggplot2 object either in the plot window or as an object with an
#' additional geom_rect layer.
#'
#' @examples
#' # combines with draw_chains to plot chains and regions.
#' data("five_rel_data")
#' p <- draw_canvas(five_rel_data)
#' p <- draw_chains(p, five_rel_data, label_size = 1.25)
#' p <- draw_regions(p, five_rel_data)
#' p
#'
#' @export
# called draw_regions
# to draw REGIONs
draw_regions <- function(p, data = data, show.legend=TRUE){
    begin=end=description=NULL
    ## plot motifs fill by description
    p <- p + ggplot2::geom_rect(data= data[data$type == "REGION",],
                        mapping=ggplot2::aes(xmin=begin,
                                xmax=end,
                                ymin=order-0.25,
                                ymax=order+0.25,
                                fill=description),
                                show.legend = show.legend)

    return(p)
}



### draw_motif
#' Add protein motifs sites to ggplot2 object.
#'
#' \code{draw_motif} adds protein motifs from Uniprot to ggplot2 object created
#' by \code{\link{draw_canvas}} and \code{\link{draw_chains}}.
#' It uses the data object.
#' The ggplot2 function \code{geom_rect} is used to draw each of the
#' motifs proportional to their number of amino acids (length).
#'
#' @param p ggplot2 object ideally created with \code{\link{draw_canvas}}.
#' @param data Dataframe of one or more rows with the following column
#' names: 'type', 'description', 'begin', 'end', 'length', 'accession',
#' 'entryName', 'taxid', 'order'. Must contain a minimum of one "CHAIN" as
#' data$type.
#' @param show.legend Option to include legend in this layer
#' @return A ggplot2 object either in the plot window or as an object with an
#' additional geom_rect layer.
#'
#' @examples
#' # combines with draw_chains to plot chains and motifs
#' data("five_rel_data")
#' p <- draw_canvas(five_rel_data)
#' p <- draw_chains(p, five_rel_data, label_size = 1.25)
#' p <- draw_motif(p, five_rel_data)
#' p
#'
#' @export
# called draw_motif
# to draw MOTIFs - no label at the moment.
draw_motif <- function(p, data = data, show.legend = TRUE){
    begin=end=description=NULL
    ## plot motifs fill by description
    p <- p + ggplot2::geom_rect(data= data[data$type == "MOTIF",],
                                mapping=ggplot2::aes(xmin=begin,
                                xmax=end,
                                ymin=order-0.25,
                                ymax=order+0.25,
                                fill=description),
                                show.legend = show.legend)

    return(p)
}


### draw_repeat
#' Add protein repeats sites to ggplot2 object.
#'
#' \code{draw_repeat} adds protein repeats from Uniprot to ggplot2 object
#' created by \code{\link{draw_canvas}} and \code{\link{draw_chains}}.
#' It uses the data object.
#' The ggplot2 function \code{geom_rect}
#' is used to draw each of the motifs proportional to their number of
#' amino acids (length).
#'
#' @param p ggplot2 object ideally created with \code{\link{draw_canvas}}.
#' @param data Dataframe of one or more rows with the following column
#' names: 'type', 'description', 'begin', 'end', 'length', 'accession',
#' 'entryName', 'taxid', 'order'. Must contain a minimum of one "CHAIN" as
#' data$type.
#' @param label_size Size of text used for labels of protein repeats.
#' @param outline Colour of the outline of each repeat.
#' @param fill Colour of the fill of each repeat.
#' @param label_repeats Option to label repeats or not.
#' @param show.legend Option to include legend in this layer
#' @return A ggplot2 object either in the plot window or as an object with an
#' additional geom_rect layer.
#'
#' @examples
#' # combines with draw_chains to plot chains and repeats.
#' data("five_rel_data")
#' p <- draw_canvas(five_rel_data)
#' p <- draw_chains(p, five_rel_data, label_size = 1.25)
#' p <- draw_repeat(p, five_rel_data)
#' p

#'
#' @export
# called draw_repeat
# to draw REPEATs & label
draw_repeat <- function(p, data = data,
                        label_size = 2,
                        outline = "dimgrey",
                        fill = "dimgrey",
                        label_repeats = TRUE,
                        show.legend = TRUE){
    begin=end=description=NULL
    ## step 6 plot repeats fill by description
    p <- p + ggplot2::geom_rect(data= data[data$type == "REPEAT",],
                        mapping=ggplot2::aes(xmin=begin,
                                xmax=end,
                                ymin=order-0.25,
                                ymax=order+0.25),
                        colour = outline,
                        fill = fill,
                        show.legend = show.legend)

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


### draw_recept_dom
#' Add receptor domains to ggplot2 object.
#'
#' \code{draw_recept_dom} adds receptor domains to the ggplot2 object created by
#' \code{\link{draw_chains}}.
#' It uses the data object.
#' The ggplot2 function \code{geom_rect} is used to draw each of the domain
#' chains proportional to their number of amino acids (length).
#'
#' @param p ggplot2 object ideally created with \code{\link{draw_canvas}}.
#' @param data Dataframe of one or more rows with the following column
#' names: 'type', 'description', 'begin', 'end', 'length', 'accession',
#' 'entryName', 'taxid', 'order'. Uses TOPO_DOM and TRANSMEM type to plot
#' these parts of receptors
#' @param label_domains Option to label receptor domains or not.
#' @param label_size Size of the text used for labels.
#' @param show.legend Option to include legend in this layer
#' @return A ggplot2 object either in the plot window or as an object with an
#' additional geom_rect layer.
#'
#' @examples
#' # combines with draw_chains to plot chains and domains.
#' data("tnfs_data")
#' p <- draw_canvas(tnfs_data)
#' p <- draw_chains(p, tnfs_data, label_size = 1.25)
#' p <- draw_recept_dom(p, tnfs_data)
#' # we like to draw receptors vertically so flip using ggplot2 functions
#'
#' p + ggplot2::scale_x_reverse() + ggplot2::coord_flip()
#'
#' @export
# called draw_recept_dom - to plot just the domains from receptors
draw_recept_dom <- function(p,
                            data = data,
                            label_domains = FALSE,
                            label_size = 4,
                            show.legend = TRUE){
    begin=end=description=NULL

    p <- p + ggplot2::geom_rect(data= data[data$type == "TOPO_DOM",],
                            mapping=ggplot2::aes(xmin=begin,
                                xmax=end,
                                ymin=order-0.25,
                                ymax=order+0.25,
                                fill=description),
                                show.legend = show.legend)

    p <- p + ggplot2::geom_rect(data= data[data$type == "TRANSMEM",],
                            mapping=ggplot2::aes(xmin=begin,
                                xmax=end,
                                ymin=order-0.25,
                                ymax=order+0.25,
                                fill=description),
                                show.legend = show.legend)

    if(label_domains == TRUE){
        p <- p + ggplot2::geom_label(data = data[data$type == "TOPO_DOM", ],
                            ggplot2::aes(x = begin + (end-begin)/2,
                                y = order,
                                label = description),
                                size = label_size)

        p <- p + ggplot2::geom_label(data = data[data$type == "TRANSMEM", ],
                            ggplot2::aes(x = begin + (end-begin)/2,
                                y = order,
                                label = "TM"),
                                size = label_size)
    }

    return(p)
}


### draw_folding
#' Add regions to ggplot object: alpha-helixes, beta-strands and turns.
#'
#' \code{draw_folding} adds alpha-helixes, beta-strands and turns to the
#' ggplot2 object created by \code{\link{draw_chains}}.
#' It uses the data object.
#' The ggplot2 function \code{geom_rect} is used to draw parts of the protein
#' chain which has alpha-helixes, beta-strands and turns proportional to the
#' number of amino acids (length).
#'
#'
#' @param p ggplot2 object ideally created with \code{\link{draw_canvas}}.
#' @param data Dataframe of one or more rows with the following column
#' names: 'type', 'description', 'begin', 'end', 'length', 'accession',
#' 'entryName', 'taxid', 'order'. Uses STRAND, HELIX and TURN type to indicate
#' these parts of the proteins.
#' @param show.legend Option to include legend in this layer
#' @param show_strand Option to show STRAND in this layer
#' @param show_helix Option to show HELIX in this layer
#' @param show_turn Option to show TURN in this layer
#'
#' @return A ggplot2 object either in the plot window or as an object with an
#' additional geom_rect layer.
#' @export
#'
#' @examples
#' # combines with draw_chains to colour chain with helicies, strands and turns.
#' data("five_rel_data")
#' p <- draw_canvas(five_rel_data)
#' p <- draw_chains(p, five_rel_data, label_size = 1.25)
#' p <- draw_folding(p, five_rel_data)
#' p
#'
#' # only colour alpha helix regions
#' p <- draw_canvas(five_rel_data)
#' p <- draw_chains(p, five_rel_data, label_size = 1.25)
#' p <-draw_folding(p, five_rel_data, show_strand = FALSE, show_turn = FALSE)
#' p
draw_folding <- function(p,
    data = data,
    show.legend = TRUE,
    show_strand = TRUE,
    show_helix = TRUE,
    show_turn = TRUE){
    begin=end=description=type=NULL
    # STRAND first
    if(show_strand == TRUE){
        p <- p + ggplot2::geom_rect(
            data = dplyr::filter(data, grepl('STRAND', type)),
            mapping=ggplot2::aes(xmin=begin,
            xmax=end,
            ymin=order-0.2,
            ymax=order+0.2,
            fill=type),
            show.legend = show.legend)
    }
    # then HELIX
    if(show_helix == TRUE){
        p <- p + ggplot2::geom_rect(data= data[data$type == "HELIX",],
            mapping=ggplot2::aes(xmin=begin,
            xmax=end,
            ymin=order-0.2,
            ymax=order+0.2,
            fill=type),
            show.legend = show.legend)
    }
    # finally TURN
    if(show_turn == TRUE){
        p <- p + ggplot2::geom_rect(data= data[data$type == "TURN",],
            mapping=ggplot2::aes(xmin=begin,
            xmax=end,
            ymin=order-0.2,
            ymax=order+0.2,
            fill=type),
            show.legend = show.legend)
    }
    return(p)
}

