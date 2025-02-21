#' A cumContribution Function
#' calculate cumulative Contribution
#' @import xts
#' @keywords cumContribution
#' @export
#' @param wealthIndex is xts object from return.portfolio
#' @param contribution is xts object from return.portfolio
#' @examples
#'#' cumContribution(wealthIndex, contribution)
# get asset contribution
cumContribution <- function (wealthIndex, contribution) {
  # portfolio_result <- globalETFPortfolio

  # raw data matrix
  pfCumReturn <- rbind(1, as.matrix(wealthIndex))
  adjPf <- as.matrix(pfCumReturn[-length(pfCumReturn), ])
  contributionMat <- as.matrix(contribution)
  adjCont <- contributionMat

  # each asset contribution
  eachAssetCont <- apply(adjCont, 2, function (x) {x * adjPf})
  eachAssetContXts <- xts::xts(eachAssetCont, order.by = index(wealthIndex))
  eachAssetCumulativeContribution <- cumsum(eachAssetContXts)

  # each asset contribution percentage
  contTotalSum <- as.matrix(rowSums(eachAssetCumulativeContribution))
  contPercentage <- apply(eachAssetCumulativeContribution, 2, function (x) {x / contTotalSum})
  contPercentageXts <- xts::xts(contPercentage, order.by = index(wealthIndex))

  out <- list("cumulative_contribution" = eachAssetCumulativeContribution,
              "contribution_percentage" = contPercentageXts)

  return(out)
}

#' A Drawdowns Function
#' calculate cumulative Contribution
#' @import xts
#' @import assertthat
#' @import PerformanceAnalytics
#' @keywords Drawdowns
#' @export
#' @param daily_return is xts object from return.portfolio
#' @examples
#'#' Drawdowns(daily_return)
# calculate drawdowns
Drawdowns <- function (daily_return) {

  assertthat::assert_that(xts::is.xts(daily_return))

  periodScale <- xts::periodicity(daily_return)$scale
  assertthat::validate_that(periodScale == "daily")

  x <- PerformanceAnalytics::checkData(daily_return)

  # Get dimensions and labels
  columns <- ncol(x)
  columnnames <- colnames(x)

  colDrawdown <- function (x) {
    cumulativeReturn <- cumprod(1 + x)
    maxCumulativeReturn <- cummax(c(1, cumulativeReturn))[-1]
    drawdown <- cumulativeReturn / maxCumulativeReturn - 1
    return(drawdown)
  }

  for (column in 1:columns) {
    columnDD <- skipNA(x[, column], FUN = colDrawdown)

    if (column == 1) {
      drawdown <- columnDD
    } else{
      drawdown <- merge(drawdown, columnDD)
    }
  }

  colnames(drawdown) <- columnnames
  drawdown <- reclass(drawdown, x)
  return(drawdown)
}


#' A skipNA Function
#' skipNA
#' @import xts
#' @import stats
#' @keywords Drawdowns
#' @export
#' @param data is xts object from return.portfolio
#' @param FUN is xts object from return.portfolio
#' @examples
#'#' skipNA(data, FUN = NULL)
# skipNA
skipNA <- function (data, FUN = NULL) {

  nx <- stats::na.omit(data)
  fx <- FUN(nx)
  if (is.vector(fx)) {
    result <- xts::.xts(fx, .index(data), .indexCLASS = indexClass(data))
  } else {
    result <- merge(fx, xts::.xts(, .index(data)))
  }
  return(result)
}

#' A mdd Function
#' mdd
#' @import xts
#' @import PerformanceAnalytics
#' @import assertthat
#' @import TTR
#' @keywords mdd
#' @export
#' @param price is xts object
#' @examples
#'#' mdd(price)
# skipNA
# calculate strategy mdd
mdd <- function (price) {
  assertthat::assert_that(xts::is.xts(price))

  minusPriceSum <- sum(price < 0)
  validate_that(minusPriceSum == 0, msg = "price should be greater than 0")

  # price can be multiple columns
  dailyROC <- TTR::ROC(price, 1, type = "discrete")
  MDD <- PerformanceAnalytics::maxDrawdown(dailyROC, invert = FALSE)

  return(t(MDD))
}

#' A pfStatistics Function
#' @import xts
#' @import PerformanceAnalytics
#' @import assertthat
#' @import TTR
#' @import quantmod
#' @import stats
#' @keywords pfStatistics
#' @export
#' @param wealth_index is xts object
#' @param cal_obj default is 'all'
#' @examples
#'#' pfStatistics(wealth_index, cal_obj = "all")
# calculate single portfolio statistics

pfStatistics <- function (wealth_index, cal_obj = "all") {
  # wealth_index <- moduleResult$result$wealthindex
  periodScale <- xts::periodicity(wealth_index)$scale
  assertthat::validate_that(periodScale == "daily")

  pfReturnDaily <- TTR::ROC(wealth_index, 1, na.pad = F, type = "discrete")
  pfReturnMonthly <- quantmod::monthlyReturn(wealth_index)
  calendarReturnTable <- PerformanceAnalytics::table.CalendarReturns(pfReturnMonthly, digits = 4, as.perc = FALSE)
  returnSign <- stats::na.omit(as.numeric(t(calendarReturnTable[, 1:12]) > 0))

  if (cal_obj == "all") {
    functionName <- c("Return.annualized", "sd.annualized", "maxDrawdown", "SharpeRatio.annualized", "SortinoRatio.annualized",
                      "min.monthly.return", "max.monthly.return", "monthly.win.ratio", "contWinFreq", "contLoseFreq")
  }

  statistics <- c()
  for (fn in functionName) {

    if (fn == "SortinoRatio.annualized") {
      value <- PerformanceAnalytics::Return.annualized(pfReturnMonthly) / (PerformanceAnalytics::DownsideDeviation(pfReturnMonthly) * sqrt(12))
    } else if (fn %in% c("Return.annualized", "SharpeRatio.annualized", "sd.annualized")) {
      value <- t(get(fn)(pfReturnMonthly))
    } else if (fn == "maxDrawdown") {
      value <- - t(get(fn)(pfReturnDaily))
    } else if (fn == "min.monthly.return") {
      value <- min(calendarReturnTable[, 1:12], na.rm = T)
    } else if (fn == "max.monthly.return") {
      value <- max(calendarReturnTable[, 1:12], na.rm = T)
    } else if (fn == "monthly.win.ratio") {
      returnFreq <- sum(is.na(calendarReturnTable[, 1:12]) != 1)
      value <- sum(returnSign) / returnFreq
    } else if (fn == "contWinFreq") {
      winRsign <- c(0, returnSign) == 1
      value <- max(diff(cumsum(winRsign)[winRsign == 0]))
    } else if (fn == "contLoseFreq") {
      loseRsign <- c(1, returnSign) == 0
      value <- max(diff(cumsum(loseRsign)[loseRsign == 0]))
    }
    statistics <- c(statistics, value)
  }
  statsMat <- as.matrix(statistics)
  rownames(statsMat) <- functionName

  return(statsMat)
}

#' A pfStatsTable Function
#' @import assertthat
#' @keywords pfStatistics
#' @export
#' @param wealth_index_table is xts object
#' @param cal_obj default is 'all'
#' @examples
#'#' pfStatsTable(wealth_index_table, cal_obj = "all")
# calculate single portfolio statistics
# get multiple portfolio statistics table
pfStatsTable <- function (wealth_index_table, cal_obj = "all") {

  assertthat::assert_that(all(wealth_index_table[1, ] == 1)) # result should be scaled to 1

  statsTable <- c()
  for (i in 1:ncol(wealth_index_table)) {
    statsTable <- cbind(statsTable, pfStatistics(wealth_index_table[,i], cal_obj))
  }
  colnames(statsTable) <- colnames(wealth_index_table)
  return(statsTable)
}

#' A returnPortfolio Function
#' @import xts
#' @import PerformanceAnalytics
#' @import assertthat
#' @import TTR
#' @import stats
#' @keywords returnPortfolio
#' @export
#' @param R is reuturns of assets
#' @param weights weight xts
#' @param wealth.index default is TRUE
#' @param contribution default is TRUE
#' @param geometric default is TRUE
#' @param rebalance_on default NA
#' @param value default is 1, starting value
#' @param verbose default is TRUE
#' @param ... Return.portfolio.geometric
# generate portfolio
returnPortfolio <- function ( R, weights = NULL, wealth.index = TRUE, contribution = TRUE, geometric = TRUE,
                              rebalance_on = c(NA, "years", "quarters", "months", "weeks", "days"),
                              value = 1, verbose = TRUE, ...) {

  R <- PerformanceAnalytics::checkData(R, method = "xts")
  if (is.null(weights) == FALSE) { R <- R[, colnames(weights)] }
  if (any(is.na(R))) {
    warning ("NA's detected: filling NA's with zeros")
    R[is.na(R)] <- 0
  }

  rebalance_on <- rebalance_on[1]
  freq <- xts::periodicity(R)
  switch ( freq$scale,
           seconds = { stop ("Use a returns series of daily frequency or higher.") },
           minute = { stop ("Use a returns series of daily frequency or higher.") },
           hourly = { stop ("Use a returns series of daily frequency or higher.") },
           daily = { time_unit = "day" },
           weekly = { time_unit = "week" },
           monthly = { time_unit = "month" },
           quarterly = { time_unit = "quarter" },
           yearly = { time_unit = "year" })

  if (time_unit == "quarter") {
    start_date <- zoo::as.yearqtr(seq(as.Date(index(R)[1]), length = 2, by = paste("-3", "month"))[2])
  } else {
    start_date <- seq(as.Date(index(R)[1]), length = 2, by = paste("-1", time_unit))[2]
  }

  if (is.null(weights)) {
    weights <- rep(1 / NCOL(R), NCOL(R))
  }

  if (is.vector(weights)) {
    if (is.na(rebalance_on)) {
      weights <- xts::xts(matrix(weights, nrow = 1), order.by = as.Date(start_date))
    } else {
      weight_dates <- c(as.Date(start_date), index(R[endpoints(R, on = rebalance_on)]))
      weights <- xts::xts(matrix(rep(weights, length(weight_dates)), ncol = NCOL(R), byrow = TRUE ),
                          order.by = as.Date(weight_dates))
    }
    colnames(weights) = colnames(R)

  } else {
    weights <- PerformanceAnalytics::checkData(weights, method = "xts")
    if (NCOL(R) != NCOL(weights)) {
      if (NCOL(R) > NCOL(weights)) {
        R <- R[, 1:NCOL(weights)]
        warning ("number of assets in beginning_weights is less than number of columns in returns, so subsetting returns.")
      } else {
        stop ("number of assets is greater than number of columns ingeometric returns object")
      }
    }
  }

  if (as.Date(xts::last(index(R))) < (as.Date(index(weights[1, ])) + 1)) {
    stop (paste("last date in series", as.Date(xts::last(index(R))),
                "occurs before beginning of first rebalancing period",
                as.Date(first(index(weights))) + 1))
  }

  if (as.Date(index(weights[1, ])) > as.Date(xts::first(index(R)))) {
    R <- R[paste0(as.Date(index(weights[1, ])) + 1, "/")]
  }

  if (geometric) {
    out <- Return.portfolio.geometric( R = R, weights = weights, wealth.index = wealth.index, contribution = contribution,
                                       rebalance_on = rebalance_on, value = value, verbose = verbose, ... = ... )
  }

  return(out)
}

#' A Return.portfolio.geometric Function
#' @import xts
#' @import PerformanceAnalytics
#' @import assertthat
#' @import TTR
#' @import stats
#' @keywords Return.portfolio.geometric
#' @export
#' @param R is reuturns of assets
#' @param weights weight xts
#' @param wealth.index default is TRUE
#' @param contribution default is TRUE
#' @param geometric default is TRUE
#' @param rebalance_on default NA
#' @param value default is 1, starting value
#' @param verbose default is TRUE
#' @param ... Return.portfolio.geometric
# generate portfolio using geometrical mean
Return.portfolio.geometric <- function(R, weights = NULL, wealth.index = TRUE, contribution = TRUE,
                                       rebalance_on = c(NA, 'years', 'quarters', 'months', 'weeks', 'days'),
                                       value = 1, verbose = TRUE, ... ) {
  # bop = beginning of period
  # eop = end of period

  # portfolio returns are only accounted for after the first rebalancing date
  R.idx = index(R[paste0(as.Date(index(weights[1,])) + 1, "/")])
  bop_value = matrix(0, NROW(R.idx), NCOL(R))
  colnames(bop_value) = colnames(R)
  eop_value = bop_value

  if (verbose | contribution) {
    period_contrib = bop_value
    if (verbose) {
      bop_weights = bop_value
      eop_weights = bop_value
    }
  }
  ret = eop_value_total = bop_value_total = vector("numeric", NROW(R.idx))

  # The end_value is the end of period total value from the prior period
  end_value <- value

  # initialize counter
  k = 1
  for (i in 1:NROW(weights)) {
    # identify rebalance from and to dates (weights[i,], weights[i+1]) and
    # subset the R(eturns) object
    from = as.Date(index(weights[i,])) + 1
    if (i == nrow(weights)) {
      to = as.Date(last(index(R))) # this is correct
    } else {
      to = as.Date(index(weights[(i + 1),]))
    }
    returns = R[paste0(from, "::", to)]
    # Only enter the loop if we have a valid returns object
    if (nrow(returns) >= 1) {
      # inner loop counter
      jj = 1
      for (j in 1:nrow(returns)) {
        # We need to know when we are at the start of this inner loop so we can
        # set the correct beginning of period value. We start a new inner loop
        # at each rebalance date.
        # Compute beginning of period values
        if (jj == 1) {
          bop_value[k,] = end_value * weights[i,]
        } else {
          bop_value[k,] = eop_value[k - 1,]
        }
        bop_value_total[k] = sum(bop_value[k,])

        # Compute end of period values
        eop_value[k,] = (1 + coredata(returns[j,])) * bop_value[k,]
        eop_value_total[k] = sum(eop_value[k,])

        if (contribution | verbose) {
          # Compute period contribution
          period_contrib[k,] = returns[j,] * bop_value[k,] / sum(bop_value[k,])
          if (verbose) {
            # Compute bop and eop weights
            bop_weights[k,] = bop_value[k,] / bop_value_total[k]
            eop_weights[k,] = eop_value[k,] / eop_value_total[k]
          }
        }

        # Compute portfolio returns
        # Could also compute this by summing contribution, but this way we
        # don't have to compute contribution if verbose=FALSE
        ret[k] = eop_value_total[k] / end_value - 1

        # Update end_value
        end_value = eop_value_total[k]

        # increment the counters
        jj = jj + 1
        k = k + 1
      }
    }
  }
  #R.idx = index(R)
  ret = xts::xts(ret, R.idx)
  colnames(ret) = "portfolio.returns"

  if (wealth.index) {
    result = cumprod(1 + ret)
    colnames(result) = "portfolio.wealthindex"
  } else {
    result = ret
  }

  if (verbose) {
    out = list()
    out$returns = ret
    out$contribution = xts::xts(period_contrib, R.idx)
    out$BOP.Weight = xts::xts(bop_weights, R.idx)
    out$EOP.Weight = xts::xts(eop_weights, R.idx)
    out$BOP.Value = xts::xts(bop_value, R.idx)
    out$EOP.Value = xts::xts(eop_value, R.idx)
    if (wealth.index) {
      out$wealthindex = result
    }
  } else if (contribution) {
    out = cbind(result, xts::xts(period_contrib, R.idx))
  } else {
    out = result
  }
  return(out)
}



#' A restrictedPartition Function
#' restrictedPartition
#' @keywords weight
#' @export
#' @param total is scalar
#' @examples
#'#' restrictedPartition(total= 100, minDeposits=c(0,0,0), depositUnits= c(5,5,5))
# skipNA
# calculate strategy mdd
restrictedPartition <- function (total, minDeposits, depositUnits, allowZero = FALSE) {
  ## portfolio weight generation function
  ## restrictedPartition(total= 100, minDeposits=c(0,0,0), depositUnits= c(5,5,5))
  if (length(depositUnits) != length(minDeposits)) {
    stop("`minDeposit` and `depositUnits` must have the same length")
  }

  nrOfPartitions <- length(depositUnits)

  if (nrOfPartitions == 0) {
    stop("zero length partition is not allowed")
  }

  run <- function (nn, mins, units, acc) {
    begin <- mins[1]
    unit <- units[1]
    remaining <- length(units)
    if (remaining == 0) {
      ret <- if (nn == 0) c(acc) else c()
      return(ret)
    }

    xs <- c()
    depositsInCurrentStep <- {
      s0 <- if (begin > nn) c() else seq(begin, nn, by=unit)
      if (begin == 0) s0 else append(0, s0)
    }

    for (deposit in depositsInCurrentStep) {
      inner <- run(nn - deposit, tail(mins, remaining - 1), tail(units, remaining - 1), append(acc, deposit))
      xs <- append(xs, inner)
    }
    return(xs)
  }
  rows <- run(total, minDeposits, depositUnits, c())
  res <- matrix(rows, ncol = nrOfPartitions, byrow = TRUE)


  if (!allowZero) {
    ncols <- ncol(res)
    for(i in 1 : ncols){
      res <- matrix(res[res[,i] >= minDeposits[i],], ncol = ncols)
    }
  }

  res
}
