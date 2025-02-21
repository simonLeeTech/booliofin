#' A dayPickInWeek Function
#' calculate dayPickInWeek
#' @import TTR
#' @import xts
#' @import assertthat
#' @keywords dayPickInWeek
#' @export
#' @param date_index is xts object
#' @param numb is number of days to average
#' @param days_in_week is number of days to average
#' @param n_week is number of days to average
#' @examples
#'#' dayPickInWeek(date_index, numb = 5, days_in_week = 5, n_week = 1)

dayPickInWeek <- function (date_index, numb = 5, days_in_week = 5, n_week = 1){

  ## dates: 'index' from xts object;
  ## numb: 1 is the first day of week
  ## days_in_week: how many days are in one week? 5 or 7 ?
  ## n_week: 1 week set 1, 2 weeks set 2
  assertthat:: assert_that(numb <= days_in_week)
  assertthat:: assert_that(is.date(date_index))

  splitxts <- split(xts:: xts(order.by = date_index), f = "weeks", k = n_week)

  splitDayIndex <- lapply(splitxts, index)
  nDay <- lapply(splitDayIndex, length)
  daygridByWeek <- lapply(nDay,
                          function (x) {
                            c(1:x) * days_in_week / x
                          })

  daygridList <- list()
  for (i in 1:length(splitDayIndex)) {
    daygridList[[i]] <- xts:: as.xts(daygridByWeek[[i]], order.by = splitDayIndex[[i]])
  }

  pickDay <- unlist(lapply(daygridList,
                           function (x) {
                             as.character(index(x)[which.min(abs(x - numb))])
                           }))
  out <- pickDay
  return(out)
}


#' A dayPickInMonth Function
#' calculate dayPickInMonth
#' @import TTR
#' @import xts
#' @import assertthat
#' @keywords dayPickInMonth
#' @export
#' @param date_index is xts object
#' @param numb is number of days to average
#' @param days_in_month is number of days to average
#' @param n_month is number of days to average
#' @examples
#'#' dayPickInMonth(date_index, numb = 21, days_in_month = 21, n_month=1)

dayPickInMonth <- function (date_index, numb = 21, days_in_month = 21, n_month=1) {

  ## dates: 'index' from xts object;
  ## numb: 1 is the first day of month
  ## days_in_month: how many days are in one month? 21 or 31 ?
  ## if you don't mind the weekends then, use daysInMonth 31
  ## n_month: 1 month set 1, 2 months set 2
  assertthat::assert_that(numb <= days_in_month)
  assertthat::assert_that(is.date(date_index))

  splitxts <- split(xts::xts(order.by = date_index), f = "months", k = n_month)

  splitDayIndex <- lapply(splitxts, index)
  nDay <- lapply(splitDayIndex, length)
  daygridByMonth <- lapply(nDay,
                           function(x) {
                             c(1:x) * days_in_month / x
                           })
  daygridList <- list()
  for (i in 1:length(splitDayIndex)) {
    daygridList[[i]] <- as.xts(daygridByMonth[[i]], order.by = splitDayIndex[[i]])
  }

  pickDay <-unlist(lapply(daygridList,
                          function (x) {
                            as.character(index(x)[which.min(abs(x - numb))])
                          }))
  out <- pickDay
  return(out)
}

#' A dayPick Function
#' calculate dayPick
#' @import TTR
#' @import xts
#' @import assertthat
#' @keywords dayPickInMonth
#' @export
#' @param date_index is xts object
#' @param split_grid is number of days to average
#' @param numb is number of days to average
#' @param days_in is number of days to average
#' @param n is number of days to average
#' @examples
#'#' dayPickInMonth(date_index, numb = 21, days_in_month = 21, n_month=1)

dayPick <- function(date_index, split_grid = c("week", "month"), numb = 21, days_in = 21, n = 1){

  if(split_grid == "month") {

    out <- dayPickInMonth(date_index, numb = numb, days_in_month = days_in, n_month = n)
  } else if(split_grid == "week") {

    out <- dayPickInWeek(date_index, numb = numb, days_in_week = days_in, n_week = n)
  }
  return(out)
}