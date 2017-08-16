#' @export
# new function
# called geom_chains to see if I can make it work
geom_chains <- function(prot_data = prot_data,
                        outline = "black",
                        fill = "grey",
                        label_chains = TRUE,
                        labels = prot_data[prot_data$type == "CHAIN",]$entryName,
                        size = 4){

    p <-ggplot2::ggplot() +
    ggplot2::ylim(0.5, max(prot_data$order)+0.5) +
    ggplot2::xlim(-max(prot_data$end)*0.2, max(prot_data$end) + max(prot_data$end)*0.1) +
    ggplot2::geom_rect(data = prot_data[prot_data$type == "CHAIN",],
                       mapping=aes(xmin=begin,
                                   xmax=end,
                                   ymin=order-0.2,
                                   ymax=order+0.2),
                       colour = outline,
                       fill = fill)

  if(label_chains == TRUE){
    p <- p +
      ggplot2::annotate("text", x = -10, y = prot_data[prot_data$type == "CHAIN",]$order,
                        label = labels,
                        hjust = 1,
                        size = size)
  }
  return(p)
}


#' @export
# called geom_domains to plot just the domains
geom_domains <- function(p){
  p <- p + ggplot2::geom_rect(data= prot_data[prot_data$type == "DOMAIN",],
            mapping=aes(xmin=begin,
                        xmax=end,
                        ymin=order-0.25,
                        ymax=order+0.25,
                        fill=description)) +
    ggplot2::geom_label(data = prot_data[prot_data$type == "DOMAIN", ],
             aes(x = begin + (end-begin)/2,
                 y = order,
                 label = description))
  return(p)
}

#' @export
# called geom_phospho
# to draw phosphorylation sites on the protein with geom_point()
geom_phospho <- function(p){
  p <- p + ggplot2::geom_point(data = drawProteins::phospho_site_info(prot_data),
                      aes(x = begin,
                          y = order+0.25),
                      shape = 21,
                      colour = "black",
                      fill = "yellow")
  return(p)
}

#' @export
# called geom_motif
# to draw MOTIFs - no label at the moment.
geom_region <- function(p){
  ## plot motifs fill by description
  p <- p + geom_rect(data= prot_data[prot_data$type == "REGION",],
                     mapping=aes(xmin=begin,
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
  p <- p + geom_rect(data= prot_data[prot_data$type == "MOTIF",],
                     mapping=aes(xmin=begin,
                                 xmax=end,
                                 ymin=order-0.25,
                                 ymax=order+0.25,
                                 fill=description))

  return(p)
}


#' @export
# called geom_repeat
# to draw REPEATs & label
geom_repeat <- function(p){
## step 6 plot repeats fill by description
p <- p + geom_rect(data= prot_data[prot_data$type == "REPEAT",],
                   mapping=aes(xmin=begin,
                               xmax=end,
                               ymin=order-0.25,
                               ymax=order+0.25))

# label repeats (for this they are ANK but remove digits)
p <- p + geom_text(data = prot_data[prot_data$type == "REPEAT",],
                   aes(x = begin + (end-begin)/2,
                       y = order,
                       label = gsub("\\d", "", description)))
return(p)
}
