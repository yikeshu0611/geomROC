#' Add Confidence Interval
#'
#' @param data results of rocit()
#' @param mapping mapping
#' @param color color for line
#' @param fill fill for polygon
#' @param alpha alpha, defaulted is 0.5
#' @param ... passed to params
#' @importFrom ggplot2 GeomPolygon
#' @details see \code{\link[ROCit]{ciAUC}}
#' @export
#'
roc_ci<-function (data,mapping = aes(),
                  color='black',
                  fill=NA,
                  alpha=0.5,...){
    roc_ci=ROCit::ciROC(data)
    low=data.frame(x=as.numeric(data$FPR),
                   y=as.numeric(roc_ci$LowerTPR))
    high=data.frame(x=as.numeric(rev(data$FPR)),
                    y=as.numeric(rev(roc_ci$UpperTPR)))
    roc_df = rbind(low,high)
    data=data.frame(roc_df)
    mapping=modifyList(mapping,aes(x = x, y = y))
    layer(data = data, mapping = mapping, stat = "identity",
          geom = GeomPolygon,
          position = "identity", show.legend = NA,
          inherit.aes = TRUE,
          params = list(na.rm = FALSE, rule = "evenodd",
                        fill=fill,color=color,alpha=alpha, ...))
}
