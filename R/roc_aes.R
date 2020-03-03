#' ROC Aesthetic
#' @name roc-aes
#' @rdname roc-aes
#' @param ... one or more colors or linetypes
#' @export
roc_aes_color<-function(...){
    color=list(...)
    values=do.call(c,color)
    scale_color_manual(values = values)
}
#' @export
#' @rdname roc-aes
roc_aes_linetype<-function(...){
    color=list(...)
    values=do.call(c,color)
    scale_linetype_manual(values = values)
}
