#' Visualize a 2x2 matrix
#'
#' @param x A named vector to visualize
#' @param labels The labels to display clockwise from topleft
#' @param colors Colors to use from topleft
#' @param labels_size Size of the labels
#'
#' @return a ggplot2 object
#'
#' @import ggplot2
#' @import dplyr
#' @importFrom gridExtra grid.arrange
#'
#' @export
#'

squares2x2 <- function(x,
											 labels = NULL,
											 colors = "white",
											 labels_size = 4) {

	m.sqrt <- sqrt(x)
	m.sqrt

	d <- data.frame(x1 = c(-m.sqrt[1], 0, -m.sqrt[4], 0),
									x2 = c(0, m.sqrt[2], 0, m.sqrt[3]),
									y1 = c(0,0,-m.sqrt[4],-m.sqrt[3]),
									y2 = c(m.sqrt[1],m.sqrt[2],0,0),
									xl = c(-m.sqrt[1]/2, m.sqrt[2]/2, m.sqrt[3]/2, -m.sqrt[4]/2),
									yl = c( m.sqrt[1]/2, m.sqrt[2]/2, -m.sqrt[3]/2, -m.sqrt[4]/2),
									cl = c(colors),
									t = names(data))


	ggplot(data = d) +
		theme_void() +
		scale_x_continuous(name = "x") +
		scale_y_continuous(name = "y") +
		geom_rect(aes(xmin = x1, xmax = x2, ymin = y1, ymax = y2),
							color = "grey50",
							fill = colors[c(1,2,4,3)],
							alpha = 0.5) +
		geom_text(aes(x = xl, y = yl, label = labels), size = labels_size) +
		theme(legend.position = 'none')

}

#' Visualize a 2x2 matrix with waffell
#'
#' @param x A named vector to visualize
#' @param labels The labels to display
#' @param colors Colors to use
#' @param labels_size Size of the labels
#'
#' @return a ggplot2 object
#'
#' @import ggplot2
#' @import dplyr
#' @importFrom gridExtra grid.arrange
#'
#' @export
#'

waffell2x2 <- function(x,
											 n = 1000,
											 colors = c("#74ADD1", "#D73027", "#D73027", "#74ADD1")) {

	p = prop.table(x)

	l <- ceiling(sqrt(max(p)*1000))

	# the grid is fixed
	g <- expand.grid(x = 1:l, y = 1:l)

	# 1
	c <- ceiling(sqrt(p[1]*1000))
	n <- ceiling(p[1]*1000)
	dta.p = data.frame(x = g$x, y = g$y) %>%
		mutate(value = ifelse((x %in% (l - c + 1):l & y <= c), "Y", "N")) %>%
		arrange(desc(value), desc(y)) %>% mutate(value = replace(value, 1:(c^2 - n), "N"))
	p1 <- ggplot(dta.p, aes(x, y, fill = value)) +
		geom_tile(color = "white", size = 1) +
		coord_fixed(expand = FALSE, clip = "off") +
		scale_x_continuous(name = NULL, breaks = NULL) +
		scale_y_continuous(name = NULL, breaks = NULL) +
		scale_fill_manual( values = c("Y" = colors[1], "N" = "white")) +
		theme(legend.position = "none")
	p1

	# 2
	c <- ceiling(sqrt(p[2]*1000))
	n <- ceiling(p[2]*1000)
	dta.p = data.frame(x = g$x, y = g$y) %>%
		mutate(value = ifelse((x <= c & y <= c), "Y", "N")) %>%
		arrange(desc(value), desc(y), desc(x)) %>% mutate(value = replace(value, 1:(c^2 - n), "N"))
	p2 <- ggplot(dta.p, aes(x, y, fill = value)) +
		geom_tile(color = "white", size = 1) +
		coord_fixed(expand = FALSE, clip = "off") +
		scale_x_continuous(name = NULL, breaks = NULL) +
		scale_y_continuous(name = NULL, breaks = NULL) +
		scale_fill_manual( values = c("Y" = colors[2], "N" = "white")) +
		theme(legend.position = "none")
	p2

	# 3
	c <- ceiling(sqrt(p[3]*1000))
	n <- ceiling(p[3]*1000)
	dta.p = data.frame(x = g$x, y = g$y) %>%
		mutate(value = ifelse((x <= c & y > (l - c)), "Y", "N")) %>%
		arrange(desc(value), desc(x)) %>% mutate(value = replace(value, 1:(c^2 - n), "N"))
	p3 <- ggplot(dta.p, aes(x, y, fill = value)) +
		geom_tile(color = "white", size = 1) +
		coord_fixed(expand = FALSE, clip = "off") +
		scale_x_continuous(name = NULL, breaks = NULL) +
		scale_y_continuous(name = NULL, breaks = NULL) +
		scale_fill_manual( values = c("Y" = colors[4], "N" = "white")) +
		theme(legend.position = "none")
	p3

	# 4
	c <- ceiling(sqrt(p[4]*1000))
	n <- ceiling(p[4]*1000)
	dta.p = data.frame(x = g$x, y = g$y) %>%
		mutate(value = ifelse((x %in% (l - c + 1):l & y > (l - c + 1)), "Y", "N")) %>%
		arrange(desc(value)) %>% mutate(value = replace(value, 1:(c^2 - n), "N"))
	p4 <- ggplot(dta.p, aes(x, y, fill = value)) +
		geom_tile(color = "white", size = 1) +
		coord_fixed(expand = FALSE, clip = "off") +
		scale_x_continuous(name = NULL, breaks = NULL) +
		scale_y_continuous(name = NULL, breaks = NULL) +
		scale_fill_manual( values = c("Y" = colors[3], "N" = "white")) +
		theme(legend.position = "none")
	p4

	gridExtra::grid.arrange(p1, p2, p4, p3, nrow = 2)

}
