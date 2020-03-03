#' Theme for ROC
#'
#' @param axis_title_size axis title size for x and y
#' @param axis_text_size axis text size for x and y
#' @param legend_text_size legend text size
#' @param legend_line_size legend key size, key type if line
#' @param axis_line_size axis line size for x and y
#' @param axis_ticks_size axis ticks size for x and y
#' @param border_size border size
#' @param family family
#' @importFrom grid unit
#' @importFrom ggplot2 %+replace% element_blank element_line element_rect element_text theme theme_grey
#'
#'
#' @export
#'
roc_theme <-function (axis_title_size = 14,
                      axis_text_size=12,
                      legend_text_size=12,
                      legend_line_size=20,
                      axis_line_size=1,
                      axis_ticks_size=0.5,
                      border_size=0,
                      family='') {
    border_size=ifelse(border_size==0,NA,border_size)
    theme_grey(base_size = axis_title_size,
               base_family = family) %+replace%
        theme(legend.title = element_blank(),
              axis.text = element_text(color = "black",
                                       size = axis_text_size,
                                       family = family),
              axis.title = element_text(color = "black",
                                        size=axis_title_size,
                                        family = family),
              panel.grid = element_blank(),
              plot.background = element_blank(),
              panel.background = element_blank(),
              panel.border = element_rect(size=border_size,fill = NA),
              axis.line = element_line(color = "black",
                                       size = axis_line_size,
                                       linetype = 1, lineend = "butt"),
              axis.ticks = element_line(color = "black",
                                        size = axis_ticks_size,
                                        linetype = 1),
              legend.key = element_blank(),
              legend.text = element_text(color = "black",
                                         size = legend_text_size,
                                         family = family),
              legend.key.size = unit(legend_line_size, "pt"),
              complete = TRUE)
}
