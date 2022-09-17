# Header ------------------------------------------------------------------
# Bivariate choropleth map in R
# using tmap
#
# Bivariate color schemes
# http://www.joshuastevens.net/cartography/make-a-bivariate-choropleth-map/
#
# Author: Stefano De Sabbata
# Date: 23 November 2018

library(sp)
library(spdep)
library(tmap)
library(classInt)
library(grid)
library(gridExtra)
library(lattice)


# Color scheme ------------------------------------------------------------
# Set the colours


bvColors <- c("#e8e8e8","#e4acac","#c85a5a",
              "#b0d5df","#ad9ea5","#985356",
              "#64acbe","#627f8c","#574249")


# bvColors <- c("#f3f3f3","#c2f0ce","#8ae1ae",
#               "#eac5dd","#9ec5d3","#7ec5b1",
#               "#e6a2d0","#bb9fce","#7a8eae")
# 

# bvColors <- c("#64acbe","#627f8c","#574249",
#           "#b0d5df","#ad9ea5","#985356",
#           "#e8e8e8","#e4acac","#c85a5a")


# 
# bvColors <- c(  "#e8e8e8", "#b0d5df","#64acbe",
#                 "#e4acac", "#ad9ea5", "#627f8c",
#                 "#c85a5a" , "#985356", "#574249")

# 
# bvColors <- c("#574249","#627f8c","#64acbe",
#               "#985356","#ad9ea5","#b0d5df",
#               "#c85a5a","#e4acac", "#e8e8e8")


# col_age <- c("#e8e8e8","#ad9ea5","#574249")

# col_age <-  c("#64acbe","#627f8c","#574249",
#               "#b0d5df","#ad9ea5","#985356",
#               "#e8e8e8","#e4acac","#c85a5a")

# bvColors <- c("#e4acac","#e4acac","#574249",
#               "#e4acac","#c85a5a","#e4acac",
# #               "#e8e8e8","#e4acac","#e4acac")
# 
# 
# bvColors <- c("#ffffe5","#fed98e", "#ec7014",
#               "#fed98e","#ec7014","#fed98e",
#               "#ec7014","#fed98e","#ffffe5")


# bvColors <-   c("#FFFFE5","#FFFFE5","#fec44f",
#                 "#FFFFE5","#ec7014","#FFFFE5",
#                 "#cc4c02","#FFFFE5","#FFFFE5")



# bvColors <- c("#FFFFE5","#FFF7BC","#FEE391",
#               "#FEC44F","#FE9929","#EC7014",
#               "#CC4C02","#993404","#662506")

# "#e8e8e8" <- weiss
# 
# bvColors <- c("#e8e8e8","#e4acac", "#574249",
#               "#e4acac","#574249","#e4acac",
#               "#574249","#e4acac","#e8e8e8")


# bvColors <- c("#ffffe5","#fed98e", "#993404",
#               "#fed98e","#993404","#fed98e",
#               "#993404","#fed98e","#ffffe5")

# bvColors <- c("#ffffe5","#fed98e", "#ec7014",
#               "#fed98e","#ec7014","#fed98e",
#               "#ec7014","#fed98e","#ffffe5")


# Print bivariate map -----------------------------------------------------
# The function below calls the function that creates the map
# then adds a square legend and prints the plot

bivariate_choropleth <- function (
    
  # Function parameters
  bivmap_dataset,         # A SpatialPoligonDataFrame
  bivmap_vars,            # A vector of characters containing the name of the two variables
  bivmap_labels=NA,       # A vector of characters containing the labels for the two variables, to use in the legend
  bivmap_size, 
  bivmap_style='quantile',# Classification type for the bins
  bivmap_scale=FALSE      # Use a scale bar
  
) {
  
  # Create the bivatiate map
  bivmap <- get_bivariate_choropleth(
    # Passs parameters on
    # except labels
    bivmap_dataset,
    bivmap_vars,
    bivmap_size,
    bivmap_style,
    bivmap_scale
  )
  
  # if (is.na(bivmap_labels)){
  #   bivmap_labels <- bivmap_vars
  # }
  # 
  # else {
  #   bivmap_labels <- bivmap_labels
  # }
  # 
  # Print map
  suppressWarnings(print( bivmap ))
  
  # Create the square legend
  vp <- viewport(x=.1, y=.8, width=.3, height=.3)
  pushViewport(vp)
  print(levelplot(
    matrix(1:9, nrow=3), 
    axes=FALSE, 
    col.regions=bvColors,
    xlab=list(label=bivmap_labels[1],cex=0.8), 
    ylab=list(label=bivmap_labels[2],cex=0.8), 
    cuts=8, 
    colorkey=FALSE,
    scales=list(draw=0)),
    newpage=FALSE)
  
  
  # Pop viewport
  popViewport()
}



# Create bivariate map ----------------------------------------------------
# This function actually creates the bivariate map using tmap

get_bivariate_choropleth <- function (
    
  # Function parameters
  bivmap_dataset,         # A SpatialPoligonDataFrame
  bivmap_vars,            # A vector of characters containing the name of the two variables
  bivmap_size,
  bivmap_style='quantile',# Classification type for the bins
  bivmap_scale=FALSE      # Use a scale bar
  
) {
  
  
  # Extract the two specified colums
  # excluding rows with na and infinite values
  #bivmap_sdf <- bivmap_dataset[
  #  !is.na(bivmap_dataset@data[, bivmap_vars[1]]) &
  #    !is.na(bivmap_dataset@data[, bivmap_vars[2]]) &
  #    !is.infinite(bivmap_dataset@data[, bivmap_vars[1]]) &
  #    !is.infinite(bivmap_dataset@data[, bivmap_vars[2]])
  #  ,bivmap_vars]
  bivmap_sdf <- bivmap_dataset[, c(bivmap_vars, bivmap_size)]
  
  # Renaming the variables to simplify the code below
  colnames(bivmap_sdf@data) <- c("xvar","yvar","Prop_Norm")
  
  # Create the 3-class categorization per each variable
  bivmap_sdf$xcat <- findCols(classIntervals( bivmap_sdf$xvar, n=3, bivmap_style))
  cat(bivmap_vars[1], "breaks (x-axis):\n")
  print(classIntervals( bivmap_sdf$xvar, n=3, bivmap_style))
  #
  bivmap_sdf$ycat <- findCols(classIntervals( bivmap_sdf$yvar, n=3, bivmap_style))
  cat(bivmap_vars[2], "breaks (y-axis):\n")
  print(classIntervals( bivmap_sdf$yvar, n=3, bivmap_style))
  
  #
  bivmap_sdf$symcat <- findCols(classIntervals( bivmap_sdf$Prop_Norm, n=3, bivmap_style))
  cat(bivmap_vars[2], "breaks (Prop_norm-axis):\n")
  print(classIntervals( bivmap_sdf$Prop_Norm, n=3, bivmap_style))
  
  # Combine the above into one 9-class categorization
  bivmap_sdf$bicat <- bivmap_sdf$xcat + (3 * (bivmap_sdf$ycat - 1))
  bivmap_sdf$bicol <- bvColors[bivmap_sdf$bicat]
  bivmap_sdf$bicol <- ifelse(is.na(bivmap_sdf$bicol), "#bdbdbd", bivmap_sdf$bicol)
  
  
  bivmap_sdf$bicats <- bivmap_sdf$symcat + (3 * ( bivmap_sdf$ycat- 1))
  bivmap_sdf$bisym <- bvColors[bivmap_sdf$bicats]
  bivmap_sdf$bisym <- ifelse(is.na(bivmap_sdf$bisym), "#bdbdbd", bivmap_sdf$bisym)
  
  # Double-check created datasets if necessary
  #View(bivmap_sdf@data)
  #View(cbind(bivmap_sdf@data, bivmap_dataset@data))
  
  # Create the map
  bivmap <- tm_shape(bivmap_sdf) + 
    # Fill
    tm_fill(
      "bicol") +
    # Remove frame
    tm_layout(frame=FALSE)+
    # Add rhe legend
    # tm_shape(bivmap_sdf) + 
     tm_symbols(col="bisym", 
              scale =1,
             border.col = "white",
             border.lwd = 1)+
    tm_legend(scale=0.75) +
    tm_borders(alpha = 0.8) 
  # tm_symbols(col="Prop_Norm", scale =2, alpha=1,style="fixed",
  #            palette = col_age, border.col = "white",
  #            labels=c("1.Tertile (low)","2.Tertile", "3.Tertile (high)"),
  #            title.col = "Age > 30 years")
  
  if (bivmap_scale) {
    bivmap <- bivmap  +
      # Add scale bar
      tm_scale_bar(
        width=0.30,
        position=c("left","bottom"))
  }
  
  # Return bivariate map
  bivmap
  
}