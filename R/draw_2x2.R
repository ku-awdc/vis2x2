#' Visualise a 2x2 matrix
#'
#' @param matrix The 2x2 matrix to visualise
#' @param labels The labels to display
#' @param colors Colors to use
#' @param labels_size Size of the labels
#'
#' @return a ggplot2 object
#' @import tidyverse
#'
#' @export
#'
squares2x2 <- function(matrix,
											 labels = NULL,
											 colors = c("#74ADD1", "#D73027", "#D73027", "#74ADD1"),
											 labels_size = 4) {

	m.sqrt <- sqrt(m)
	m.sqrt

	d <- data.frame(x1 = c(-m.sqrt[1],0,-m.sqrt[2],0),
									x2 = c(0,m.sqrt[3],0,m.sqrt[4]),
									y1 = c(0,0,-m.sqrt[2],-m.sqrt[4]),
									y2 = c(m.sqrt[1],m.sqrt[3],0,0),
									t = c(dimnames(m)[[1]][1],
												dimnames(m)[[1]][2],
												dimnames(m)[[2]][1],
												dimnames(m)[[2]][2]))
	ggplot() +
		theme_void() +
		scale_x_continuous(name = "x") +
		scale_y_continuous(name = "y") +
		geom_rect(data = d, mapping = aes(xmin = x1, xmax = x2, ymin = y1, ymax = y2),
							color = "grey50",
							fill = colors,
							alpha = 0.5) +
		geom_text(data = d, aes(x = x1 + (x2 - x1)/2, y = y1 + (y2 - y1)/2, label = labels), size = labels_size) +
		theme(legend.position = 'none')

}
