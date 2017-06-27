#' @export

# first plotting function 

# this function should work to draw a horizontal domain structure of a protein
# it works to draw a single protein. 
# needs to be tested robustly. 

draw_mol_horiz <- function(names, features){
  ## step 3: draw the diagram
  screen.width <- max(features$end)
  screen.height <- 25  # this is a bit arbitary
  plot(c(-10, screen.width), 
       c(0, screen.height), 
       type= "n", 
       xlab = "Number of amino acids", 
       ylab = "", yaxt='n')    # suppress the y label and y axis
  # scaled.mol.sizes <- mol.sizes/screen.width
  i <- 3
  # make the rectangles in a loop
  for (i in 1:length(features$type) ) {
    rect(xleft   = features$begin[i],
         ytop    = screen.height/2 + 2.5,
         ybottom = screen.height/2 - 2.5,
         xright  = features$end[i],
         col = features$col[i])
  }
  
  # add text to the top of the illustration with the recommended name
  text(max(features$end)/2, screen.height-2.5, names$protein.recommendedName.fullName, cex=1.5)
  # and the alternative name
  text(max(features$end)/2, screen.height-5, names$protein.recommendedName.alternativeName, cex=1)
  
  # add the descriptions of the features
  pos.text.x <- features$begin[2:5] + (features$end[2:5] - features$begin[2:5])/2
  pos.text.y <- c(screen.height/2 + 3.5, screen.height/2 - 3.5)
  text(pos.text.x, pos.text.y, features$description[2:5], cex=1, col=features$col[2:5])
  
  # add the accession number to the bottom smaller text and the source of the data
  text(max(features$end)/2, 5 , paste("Uniprot Accession Number:", names$accession), cex=0.8)
  text(max(features$end)/2, 3 , "Source of data: http://www.uniprot.org/uniprot/Q04206", cex=0.8)
}


#' @export

# plotting function to add circles representing phosphorylation sites
# needs to be tested robustly. 
# default colour is yellow and radius = 10 but can be changed. 

draw_phosphosites <- function(phospho_loc, radius = 10, colour = "yellow"){
  screen.height <- 25  # this is a bit arbitary but is same as used in previous function
  for (i in 1:nrow(phospho_loc) ) {
    plotrix::draw.circle(phospho_loc$begin[i],
                         screen.height/2 + 2.5,
                         radius=radius,
                         nv=100,
                         border=NULL,
                         col=colour,  # colour could be altered. 
                         lty=1,
                         lwd=1)
  }
}