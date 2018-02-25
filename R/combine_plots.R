#' @title Combining multiple plots using `cowplot::plot_grid()` with a title and a caption
#' @name combine_plots
#' @author Indrajeet Patil
#' @description Wrapper around `cowplot::plot_grid()` that will return a plot grid along with a title and a caption
#' @return Combined plot with title and caption
#'
#' @param ... Additional arguments used in the function `cowplot::plot_grid()`.
#' @param title.text String or plotmath expression to be drawn as title for the *combined plot*.
#' @param caption.text String or plotmath expression to be drawn as the captionfor the *combined plot*.
#' @param title.colour Text color for title.
#' @param caption.colour Text color for caption.
#' @param caption.size Point size of title text.
#' @param title.size Point size of caption text.
#' @param caption.vjust Vertical justification for caption. Default = 0.5 (centered on y).
#' 0 = baseline at y, 1 = ascender at y.
#' @param title.vjust Vertical justification for title. Default = 0.5 (centered on y).
#' 0 = baseline at y, 1 = ascender at y.
#' @param caption.hjust Horizontal justification for caption. Default = 0.5 (centered on x).
#' 0 = flush-left at x, 1 = flush-right.
#' @param title.hjust Horizontal justification for title. Default = 0.5 (centered on x).
#' 0 = flush-left at x, 1 = flush-right.
#' @param caption.rel.heights Numerical vector of relative columns heights while combining (title, plot, caption).
#' @param title.rel.heights Numerical vector of relative columns heights while combining (title, plot).
#'
#' @import cowplot
#'
#' @examples
#' # loading the necessary libraries
#' library(ggplot2)
#'
#' # preparing the first plot
#' p1 <-
#'   ggplot2::ggplot(data = subset(iris, iris$Species == "setosa"),
#'                   aes(x = Sepal.Length, y = Sepal.Width)) + geom_point() +
#'   labs(title = "setosa")
#'
#' # preparing the second plot
#' p2 <-
#'   ggplot2::ggplot(data = subset(iris, iris$Species == "versicolor"),
#'                   aes(x = Sepal.Length, y = Sepal.Width)) + geom_point() +
#'   labs(title = "versicolor")
#'
#' # combining the plot with a title and a caption
#' combine_plots(
#'   p1,
#'   p2,
#'   labels = c("(a)", "(b)"),
#'   title.text = "Dataset: Iris Flower dataset",
#'   caption.text = "Note: Only two species of flower are displayed",
#'   title.colour = "red",
#'   caption.colour = "blue"
#' )
#'
#' @export


combine_plots <-
  function(...,
           title.text = NULL,
           caption.text = NULL,
           title.colour = "blue",
           caption.colour = "black",
           caption.size = 10,
           title.size = 16,
           caption.vjust = 0.5,
           caption.hjust = 0.5,
           title.vjust = 0.5,
           title.hjust = 0.5,
           title.rel.heights = c(0.1, 1.2),
           caption.rel.heights  = c(0.1, 1.2, 0.1)) {
    # preparing the basic plot
    plot <- cowplot::plot_grid(...)

    # preparing the title
    if (!is.null(title.text)) {
      title <-
        cowplot::ggdraw() +
        cowplot::draw_label(
          label = title.text,
          fontface = "bold",
          colour = title.colour,
          size = title.size,
          vjust = title.vjust,
          hjust = title.hjust
        )

      # preparing the caption
      if (!is.null(caption.text)) {
        caption <-
          cowplot::ggdraw() +
          cowplot::draw_label(
            label = caption.text,
            colour = caption.colour,
            size = caption.size,
            vjust = caption.vjust,
            hjust = caption.hjust
          )

        # combining the basic plot with the title and the caption
        plot <-
          cowplot::plot_grid(title,
            plot,
            caption,
            ncol = 1,
            rel_heights = caption.rel.heights
          )
      } else {
        # combining the basic plot with the title
        plot <-
          cowplot::plot_grid(title,
            plot,
            ncol = 1,
            rel_heights = title.rel.heights
          )
      }
    }

    # return the comibined plot with a title and caption
    return(plot)
  }
