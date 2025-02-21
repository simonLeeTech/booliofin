#' A checkDateFormat Function
#' checkDateFormat
#' @keywords checkDateFormat
#' @export
#' @param date_index is xts object
#' @examples
#'#' checkDateFormat(date_index)
# date format
checkDateFormat <- function (date_index) {
  dateIndex <- as.character(date_index)

  firstDate <- first(dateIndex)
  dateFormat <- c("ymd", "ydm", "mdy", "myd", "dmy", "dym", "yq")
  dateFunction <- unlist(lapply(dateFormat,
                                function (x) {
                                  dateType <- get(x)(firstDate)
                                  if (is.na(dateType) != TRUE) {x}
                                }))
  dateForm <- get(dateFunction)(dateIndex)
  return(dateForm)
}

#' A scalePrice Function
#' scalePrice
#' @import TTR
#' @import xts
#' @import assertthat
#' @keywords scalePrice
#' @export
#' @param price is xts object
#' @examples
#'#' checkDateFormat(date_index)
# date format
# scaling price to 1
scalePrice <- function (price) {
  assertthat::assert_that(xts::is.xts(price))

  # price can be multiple columns
  dateIndex <- index(price)
  dailyroc <- TTR::ROC(price, 1, type = "discrete")
  dailyroc[1,] <- 0
  nCol <- ncol(price)

  scaledPriceList <- list()
  for (nc in 1:nCol) {
    scaledPriceList[[nc]] <- cumprod(1 + dailyroc[, nc])
  }

  scaledPrice <- do.call(xts::merge.xts, scaledPriceList)

  return(scaledPrice)
}

#' A readCSVtoXTS Function
#' scalePrice
#' @import xts
#' @import assertthat
#' @keywords scalePrice
#' @export
#' @param file_path is xts object
#' @param col_select is xts object
#' @examples
#'#' readCSVtoXTS(file_path, col_select = c("date", "close"))
# read csv data and change data class to xts
readCSVtoXTS <- function (file_path, col_select = c("date", "close")) {

  assertthat::assert_that("csv" %in% unlist(strsplit(file_path, "[.]")), msg = "This file is not *.csv file")
  selected_column <- length(col_select)
  assertthat::validate_that(selected_column >= 2)

  rawData <- read.csv(file_path , header=T, fileEncoding = "UTF-8")
  dayData <- as.character(rawData[, col_select[1]])

  # define date format
  firstDate <- as.character(first(dayData))
  dateFormat <- c("ymd","ydm","mdy","myd","dmy","dym","yq")
  dateFunction <- unlist(lapply(dateFormat,
                                function (x) {
                                  datetype <- get(x)(firstDate)
                                  if (is.na(datetype) != TRUE) { x } }
  ))

  dateCol <- get(dateFunction)(dayData)

  if ("..." %in% col_select) {
    coreDatacol <- !colnames(rawData) %in% col_select[1]
  } else {
    coreDatacol <- colnames(rawData) %in% col_select[-1]
  }
  coreDataSet <- rawData[, coreDatacol]
  rawDataXTS <- xts::xts(coreDataSet, order.by = dateCol)

  return(rawDataXTS)
}


#' A changeStr Function
#' changeStr
#' @keywords changeStr
#' @export
#' @param string strings
#' @examples
#'#' changeStr(string)
# change string 'special character' to '.'
changeStr <- function (string) {

  nameString <- strsplit(string, "")
  newName <- unlist(lapply(nameString,
                           function (x) {
                             x[which(!is.na(match( x, c("-", "(", ")", "[", "]", " ", "/", "&") )))] <- "."
                             paste0(x, collapse = "")
                           }))
  return(newName)
}


#' A savelisttoxlsx Function
#' scalePrice
#' @import openxlsx
#' @import assertthat
#' @keywords list
#' @export
#' @param data_list list of data.frames
#' @param file_name save file directory
#' @examples
#'#' save_list_to_xlsx(data_list, file_name)
# save list with data.frame to xlsx file with multiple sheets
save_list_to_xlsx <- function(data_list, file_name){
  wb <- createWorkbook()
  datas <- data_list #list(USArrests, USArrests * 2)
  sheetnames <- paste0("Sheet", seq_along(datas)) # or names(datas) if provided
  sheets <- lapply(sheetnames, createSheet, wb = wb)
  void <- Map(addDataFrame, datas, sheets)
  saveWorkbook(wb, file = file_name)
}
