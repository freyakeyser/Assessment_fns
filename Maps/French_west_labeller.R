# Use this function within ggplot to make your longitude turn into an O instead of a W.
# Reprex below the function, but simple usage once you've created a ggplot using geom_sf()
#p <- scale_x_continuous(labels = french.west)

french.west <- function(x) {sapply(x, function(val) {
  if (val < 0) {
    paste0(abs(val), "°O")  # Ouest instead of W
  } else if (val > 0) {
    paste0(val, "°E")       # Est (still E)
  } else { "0°" }})}

