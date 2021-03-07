#' Visualize a 2x2 matrix
#'
#' @param matrix The 2x2 matrix to visualize
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

#' Visualize a 2x2 matrix with waffell
#'
#' @param matrix The 2x2 matrix to visualize
#' @param labels The labels to display
#' @param colors Colors to use
#' @param labels_size Size of the labels
#'
#' @return a ggplot2 object
#' @import tidyverse
#'
#' @export
#'
waffell2x2 <- function(matrix,
											 n = 1000,
											 colors = c("#74ADD1", "#D73027", "#D73027", "#74ADD1")) {

	p = prop.table(m)

	l <- ceiling(sqrt(max(p)*1000))

		# the grid is fixed
	g <- expand.grid(x = 1:l, y = 1:l)

	# 1,1
	c <- ceiling(sqrt(p[1, 1]*1000))
	n <- ceiling(p[1, 1]*1000)
	dta.p = data.frame(x = g$x, y = g$y) %>%
		mutate(value = ifelse((x <= c & y < c) | (x %in% c:((c^2-n)+1) & y == c), "Y", "N"))
	p11 <- ggplot(dta.p, aes(x, y, fill = value)) +
		geom_tile(color = "white", size = 1) +
		coord_fixed(expand = FALSE, clip = "off") +
		scale_x_continuous(name = NULL, breaks = NULL) +
		scale_y_continuous(name = NULL, breaks = NULL) +
		scale_fill_manual( values = c("Y" = colors[1], "N" = "white")) +
		theme(legend.position = "none")
	p11


	# 1,2
	c <- ceiling(sqrt(p[1,2]*1000))
	n <- ceiling(p[1,2]*1000)
	dta.p = data.frame(x = g$x, y = g$y) %>%
		mutate(value = ifelse((x <= c & y < c) | (x %in% 1:(c-(c^2-n)) & y == c), "Y", "N"))
	p12 <- ggplot(dta.p, aes(x, y, fill = value)) +
		geom_tile(color = "white", size = 1) +
		coord_fixed(expand = FALSE, clip = "off") +
		scale_x_continuous(name = NULL, breaks = NULL) +
		scale_y_continuous(name = NULL, breaks = NULL) +
		scale_fill_manual( values = c("Y" = colors[2], "N" = "white")) +
		theme(legend.position = "none")
	p12

	# 2,1
	c <- ceiling(sqrt(p[2,1]*1000))
	n <- ceiling(p[2,1]*1000)
	dta.p = data.frame(x = g$x, y = g$y) %>%
		mutate(value = ifelse((x > (l-c) & y > (l-c) | (x %in% l:((c^2-n)+1) & y == (l-c))), "Y", "N"))
	p21 <- ggplot(dta.p, aes(x, y, fill = value)) +
		geom_tile(color = "white", size = 1) +
		coord_fixed(expand = FALSE, clip = "off") +
		scale_x_continuous(name = NULL, breaks = NULL) +
		scale_y_continuous(name = NULL, breaks = NULL) +
		scale_fill_manual( values = c("Y" = colors[3], "N" = "white")) +
		theme(legend.position = "none")
	p21

	# 2,2
	c <- ceiling(sqrt(p[2,2]*1000))
	n <- ceiling(p[2,2]*1000)
	dta.p = data.frame(x = g$x, y = g$y) %>%
		mutate(value = ifelse((x < c & y > (l-c+1) | (x %in%  1:(n-(c-1)^2) & y == (l-c+1))), "Y", "N"))
	p22 <- ggplot(dta.p, aes(x, y, fill = value)) +
		geom_tile(color = "white", size = 1) +
		coord_fixed(expand = FALSE, clip = "off") +
		scale_x_continuous(name = NULL, breaks = NULL) +
		scale_y_continuous(name = NULL, breaks = NULL) +
		scale_fill_manual( values = c("Y" = colors[4], "N" = "white")) +
		theme(legend.position = "none")
	p22

	gridExtra::grid.arrange(p11,p12,p21,p22)

}
