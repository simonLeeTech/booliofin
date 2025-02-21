#' A laggedPrice Function
#' calculate laggedPrice
#' @import TTR
#' @import xts
#' @import assertthat
#' @import stats
#' @keywords laggedPrice
#' @export
#' @param price_1 is xts object
#' @param price_2 is xts object
#' @param merge_by is xts object
#' @examples
#'#' laggedPrice(price_1, price_2, merge_by = 1)
laggedPrice <- function (price_1, price_2, merge_by = 1) {
  # return lagged price table
  assertthat:: assert_that(xts::is.xts(price_1) & is.xts(price_2))
  negativeIndexPriceSum <- sum(price_1 <= 0)
  assertthat::validate_that(negativeIndexPriceSum == 0, msg = "index price should be greater than 0")

  negativeFundPriceSum <- sum(price_2 <= 0)
  assertthat::validate_that(negativeFundPriceSum == 0, msg = "fund price should be greater than 0")
  #- periodicity test
  # possible period type is 'daily', 'weekly', 'monthly'
  price_1Scale <- xts::periodicity(price_1)$scale
  price_2Scale <- xts::periodicity(price_2)$scale
  assertthat::assert_that(price_1Scale == price_2Scale, msg = "price data periodicity does not match (index and fund price)")
  #- possible return type test
  assertthat::assert_that(price_1Scale %in% c("daily", "weekly", "monthly"))

  ## calculate correlation
  if (merge_by == 1) {
    mergeData <- xts::merge.xts( price_2, price_1, join = "inner" )
  } else {
    mergeData <- xts::merge.xts(price_2, price_1, join = "outer")
  }

  dailyROC <- TTR::ROC(mergeData, 1, na.pad = F, type = "discrete")
  acfMat <- stats::acf(dailyROC, type = "correlation", plot = F)
  values <- acfMat$acf[,, ncol(dailyROC)]
  lagDay <- unlist(lapply(apply(as.matrix(values[, 1:(ncol(values)-1)]) > 0.5 , 2, which), max)) - 1

  ## reflect lag day
  if (length(lagDay) == 0){
    laggedPrice_xts <- mergeData
  } else {

    price_1Data <- coredata(mergeData[,colnames(price_1)])
    price_2Data <- coredata(mergeData[,colnames(price_2)])

    price_1Lagged <- price_1Data[-c(nrow(price_1Data):(nrow(price_1Data)-lagDay+1)), ]
    price_2Lagged <- price_2Data[-c(1:lagDay), ]

    laggedPriceBind <- cbind(price_1Lagged, price_2Lagged)
    colnames(laggedPriceBind) <- c(colnames(price_1), colnames(price_2))

    laggedPrice_xts <- xts::xts(laggedPriceBind, order.by = index(mergeData)[-c(1:lagDay)])
  }

  return(laggedPrice_xts)
}

#' A laggedCorrelation Function
#' calculate laggedCorrelation
#' @import TTR
#' @import xts
#' @import assertthat
#' @import stats
#' @import utils
#' @keywords laggedCorrelation
#' @export
#' @param price_table is xts object
#' @examples
#'#' laggedCorrelation(price_table)
laggedCorrelation <- function (price_table) {
  # returns lagged price applied correlation

  assertthat::validate_that(sum(price_table <= 0) == 0, msg = "price should be greater than 0.")

  possiblePairs <- utils::combn(ncol(price_table), 2)
  correlationMat <- matrix(NA, nrow = ncol(price_table), ncol = ncol(price_table))
  colnames(correlationMat) <- rownames(correlationMat) <- colnames(price_table)
  for (i in 1:ncol(possiblePairs)) {

    thisPair <- possiblePairs[,i]
    thisPrice <- price_table[,thisPair]
    laggedPriceResult <- laggedPrice(thisPrice[,1], thisPrice[,2])
    dailyROC <- TTR::ROC(laggedPriceResult, 1, na.pad = F, type = "discrete")
    correlationValue <- stats::cor(coredata(dailyROC), method = "pearson")[1,2]

    thisDataName <- colnames(thisPrice)
    correlationMat[thisDataName[1], thisDataName[2]] <- correlationValue
  }
  diag(correlationMat) <- 1
  return(correlationMat)
}