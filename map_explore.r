# load packages ----
library(ggplot2)

plot(x = c(1,2,3), y = c(4,5,6))

img <- png::readPNG("img/MegaManII-AirMan(Enemies).png")
par(mar = rep(0,4))
plot.new()
grid::grid.raster(img, x = 0.5, y = 0.5)

plot(x = c(1,2,3), y = c(4,5,6))
