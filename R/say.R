#' A say Function
#'
#' This function allows you to express your saying bye
#'
#' @keywords say
#' @export
#' @param state is true default
#' @examples
#' say()

say <- function(state = TRUE){
  if(state){
    hello()
  } else {
    bye()
  }
}

#' A sayROC Function
#'
#' This function allows you to express your saying bye
#' @import TTR
#' @keywords say
#' @param state is true default
#' @export
#' @examples
#' sayROC()
sayROC <- function(state = TRUE){
 print( TTR::ROC(c(1:10), 1))
  print("ddd")
}