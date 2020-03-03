#' Diagonal
#'
#' @param mapping mapping
#' @param ... other function passed to params
#' @param color line color, black defaulted
#' @param size line size, 0.5 defaulted
#' @param linetype line type, numeric object, 2 is defaulted
#'
#' @return A diagonal
#' @export
roc_diagonal <- function(mapping = aes(),
                         color='grey',
                         size=1.05,
                         linetype=2,...) {
    stat = "identity"
    position = "identity"
    inherit.aes = TRUE
    show.legend = NA
    na.rm = FALSE
    data=data.frame(x=c(0,1),y=c(0,1))
    mapping=modifyList(mapping,aes_string(x="x",y="x"))
    layer(data = data, mapping = mapping, stat = stat, geom = GeomLine,
          position = position, show.legend = show.legend, inherit.aes = inherit.aes,
          params = list(na.rm = na.rm,
                        color=color,
                        size=size,
                        linetype=linetype,...))
}
