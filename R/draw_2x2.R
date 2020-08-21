#' Visualise a 2x2 matrix
#'
#' @param matrix The 2x2 matrix to visualise
#'
#' @return a ggplot2 object
#' @import ggplot2

#' @export
draw_2x2 <- function(matrix){

	## Just some temporary code as a placeholder:
	df <- data.frame(
		gp = factor(rep(letters[1:3], each = 10)),
		y = rnorm(30)
	)
	ggplot(df, aes(gp, y)) +
		geom_point()


}
