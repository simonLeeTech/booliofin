# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

#' A hello Function
#'
#' This function allows you to express your saying hello
#'
#' @keywords hello
#' @export
#' @examples
#' hello()

hello <- function() {
  print("Hello, world!!")
}


#' A bye Function
#'
#' This function allows you to express your saying bye
#'
#' @keywords bye
#' @export
#' @examples
#' bye()

bye <- function(){
  print("bye bye")
}


#' A Cat Function
#'
#' This function allows you to express your love of cats.
#' @param love Do you love cats? Defaults to TRUE.
#' @keywords cats
#' @export
#' @examples
#' cat_function()

cat_function <- function(love=TRUE){
  if(love==TRUE){
    print("I love cats!")
  }
  else {
    print("I am not a cool person.")
  }
}