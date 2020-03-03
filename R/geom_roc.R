#' Geometry for ROC
#'
#' @param data roc analysis result after 'pROC' or 'ROCit' package
#' @param mapping mapping
#' @param size line size defaule is 1
#' @param ... other attributes
#' @importFrom ggplot2 GeomLine aes layer scale_color_manual scale_linetype_manual aes_string
#' @importFrom utils modifyList
#' @importFrom ROCit rocit
#' @importFrom pROC roc
#' @return gemeotry of roc
#' @export
#'
#' @examples
#' # do three ROC analysis
#' library(geomROC)
#' r1 <- rocit(score = Diabetes$bmi, class = Diabetes$dtest)
#' r2 <- rocit(score = Diabetes$chol, class = Diabetes$dtest)
#' r3 <- rocit(score = Diabetes$hdl, class = Diabetes$dtest)
#'
#' #plot one roc curve
#' ggplot()+
#'     geom_roc(r1)
#' #plot two roc curves
#' ggplot()+
#'     geom_roc(r1)+
#'     geom_roc(r2)
#' #plot three roc curves
#' ggplot()+
#'     geom_roc(r1)+
#'     geom_roc(r2)+
#'     geom_roc(r3)
#'
#' #use color
#' #add color to one roc
#' ggplot()+
#'     geom_roc(r1,aes(color='bmi'))+
#'     geom_roc(r2)+
#'     geom_roc(r3)
#' #add color to all
#' ggplot()+
#'     geom_roc(r1,aes(color='bmi'))+
#'     geom_roc(r2,aes(color='chol'))+
#'     geom_roc(r3,aes(color='hdl'))
#'
#' #use linetype
#' ggplot()+
#'     geom_roc(r1,aes(linetype='bmi'))+
#'     geom_roc(r2,aes(linetype='chol'))+
#'     geom_roc(r3,aes(linetype='hdl'))
#'
#' #use roc_theme()
#' ggplot()+
#'     geom_roc(r1,aes(linetype='bmi'))+
#'     geom_roc(r2,aes(linetype='chol'))+
#'     geom_roc(r3,aes(linetype='hdl'))+
#'     roc_theme()
geom_roc <- function(data = NULL,
                     mapping = aes(),
                     size=1,
                     ...) {
    stat = "identity"
    position = "identity"
    na.rm = FALSE
    show.legend = NA
    inherit.aes = TRUE
    if (class(data)=='rocit'){
        data=data.frame(`1-Specificity (FPR)`=data$FPR,
                        `Sensitivity (TPR)`=data$TPR,check.names = FALSE)
    }else if(class(data)=='roc'){
        data=data.frame(`1-Specificity (FPR)`=1-data$specificities,
                        `Sensitivity (TPR)`=data$sensitivities,check.names = FALSE)
    }
    mapping=modifyList(mapping,aes_string(x="`1-Specificity (FPR)`",
                                          y="`Sensitivity (TPR)`"))
    layer(data = data, mapping = mapping, stat = stat, geom = GeomLine,
          position = position, show.legend = show.legend, inherit.aes = inherit.aes,
          params = list(na.rm = na.rm,size=size, ...))
}
