#' A movingAverage Function
#' calculate moving average
#' @import TTR
#' @import xts
#' @import assertthat
#' @keywords say
#' @export
#' @param price is xts object
#' @param n is number of days to average
#' @examples
#'#' movingAverage(price, n = 200)

movingAverage <- function(price, n) {
  assertthat::assert_that(xts::is.xts(price))

  maEach <- list()
  for( cl in 1:ncol(price) ){
    maEach[[cl]] <- TTR::SMA(price[, cl], n)
  }
  ma <- do.call(xts::merge.xts, maEach)
  colnames(ma) <- colnames(price)
  return(ma)
}

#' A tsMomentumScore Function
#' calculate tmomentum score
#' @import TTR
#' @import xts
#' @import assertthat
#' @keywords tsMomentumScore
#' @export
#' @param price is xts object
#' @param mom_period vector of month
#' @param split_grid date grid, default is 'month'
#' @param ... parameters for dayPick
#' @examples
#'#' tsMomentumScore(price, mom_period= c(1:12), split_grid = "month")

tsMomentumScore <- function (price, mom_period, split_grid = "month", ...){
  assertthat::assert_that(xts::is.xts(price))

  gridDates <- dayPick(date_index = index(price), split_grid = split_grid, ...)
  # gridDates <- dayPick(date_index = index(price), split_grid = split_grid)

  forSeq <- (max(mom_period) + 1):length(gridDates)
  tmom <- xts::xts(matrix(NA, nrow = length(gridDates), ncol = ncol(price)),
                   order.by = as.Date(gridDates))
  for (cl in 1:ncol(price)) {

    thisPrice <- na.omit(price[, cl])
    for (sq in forSeq) {
      cutDate <- unlist(lapply(mom_period,
                               function (p) {
                                 paste0(gridDates[sq-p], "/", gridDates[sq])
                               }))
      scores <- unlist(lapply(cutDate,
                              function (cd) {
                                periodPrice <- thisPrice[cd]
                                as.numeric(coredata(last(periodPrice)) / coredata(first(periodPrice)) - 1)
                              }))
      tmom[sq, cl] <- mean(scores)
    }
  }
  colnames(tmom) <- colnames(price)
  out <- na.locf(tmom)
  return(out)
}

#' A priceRankCorr Function
#' calculate priceRankCorr
#' @import TTR
#' @import xts
#' @import assertthat
#' @import stats
#' @keywords priceRankCorr
#' @export
#' @param price is xts object
#' @param mom_period vector of month
#' @param split_grid date grid, default is 'month'
#' @param ... parameters for dayPick
#' @examples
#'#' priceRankCorr(price, mom_period= c(1:12), split_grid = "month")

priceRankCorr <- function (price, mom_period = c(1:12), split_grid = "month", ...) {
  assertthat::assert_that(xts::is.xts(price))

  gridDates <- dayPick(date_index = index(price), split_grid = split_grid, ...)
  #gridDates <- dayPick(date_index = index(price), split_grid = split_grid)

  forSeq <- (max(mom_period)+1):length(gridDates)
  rankCorr <- xts::xts(matrix(NA, nrow = length(gridDates), ncol = ncol(price)),
                       order.by = as.Date(gridDates))
  for (cl in 1:ncol(price)) {

    thisPrice <- na.omit(price[,cl])
    for (sq in forSeq) {
      cutDate <- unlist(lapply(mom_period,
                               function (p) {
                                 paste0(gridDates[sq-p], "/", gridDates[sq])
                               }))
      scores <- unlist(lapply(cutDate,
                              function (cd) {
                                periodPrice <- thisPrice[cd]
                                # cov(rank(coredata(periodPrice)), 1:length(periodPrice))
                                cor(rank(coredata(periodPrice)), 1:length(periodPrice))
                              }))
      rankCorr[sq, cl] <- mean(scores)
    }
  }
  colnames(rankCorr) <- colnames(price)
  out <- na.locf(rankCorr)
  return(out)
}

#' A momentumRank Function
#' calculate momentumRank
#' @import xts
#' @import assertthat
#' @import zoo
#' @keywords momentumRank
#' @export
#' @param momentum_score is score
#' @param rank_method default is 'min'
#' @examples
#'#' momentumRank(momentum_score=NULL, rank_method = "min")

momentumRank <- function(momentum_score, rank_method = "min"){
  assertthat:: assert_that(xts::is.xts(momentum_score))
  assertthat:: assert_that( ncol(momentum_score) > 1 )

  mrank <- t(apply(matrix(momentum_score, ncol = ncol(momentum_score)), 1,
                   function (x) {
                     rowRank <- rank(-x, ties.method = rank_method )
                     rowRank[which(is.na(x))] <- NA
                     rowRank }))

  colnames(mrank) <- colnames(momentum_score)
  out <- xts:: xts(mrank, order.by = index(momentum_score))
  return(out)
}


#' A endMonthRollingReturn Function
#' calculate endMonthRollingReturn
#' @import xts
#' @import assertthat
#' @import zoo
#' @keywords endMonthRollingReturn
#' @export
#' @param data is xts
#' @param roll_month default is 24
#' @examples
#'#' endMonthRollingReturn(data, roll_month = 24)
endMonthRollingReturn <- function (data, roll_month = 24) {
  # monthly switching rolling return
  assertthat::assert_that(xts::is.xts(data))

  endOfMonthIndex <- dayPickInMonth(zoo::index(data))
  allSeq <- length(endOfMonthIndex)

  existingIndex <- endOfMonthIndex[roll_month:allSeq]

  rollingReturn <- xts(matrix(NA, ncol = 1, nrow = length(existingIndex)), order.by = as.Date(existingIndex))
  for (i in allSeq:roll_month) {

    # cut period data
    rollingCutPeriod <- paste0(as.Date(endOfMonthIndex[i - roll_month]), "/", endOfMonthIndex[i])
    rollPeriodPf <- scalePrice(data[rollingCutPeriod])

    rollingReturn[i - (roll_month - 1), ] <- as.vector(coredata(xts::last(rollPeriodPf) - 1))
  }

  return(rollingReturn)
}
