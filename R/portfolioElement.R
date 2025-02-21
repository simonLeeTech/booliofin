#' A determinInvestAsset Function
#' determin
#' @import TTR
#' @import xts
#' @keywords say
#' @export
#' @param par_list is list of asset parameters
#' @param price is xts object
#' @param momentum_function tsMomentumScore, priceRankCorr
#' @examples
#'#' determinInvestAsset(par_list, price, momentum_function = "tsMomentumScore")
determinInvestAsset <- function (par_list, price, momentum_function = c("tsMomentumScore", "priceRankCorr")) {
  # par_list <- list("asset" = c("bond2"),
  #               "top_rank" = 2,
  #               "mom_period" = c(6),
  #               "asset_weight" = c(0.2),
  #               "ma_period" = NULL)
  momScore <- get(momentum_function)(price, mom_period = par_list$mom_period)
  if (ncol(momScore) == 1) {
    momRanking <- xts::xts(rep(1, nrow(momScore)), order.by = index(momScore))
  } else {
    momRanking <- momentumRank(momScore)
  }


  # sma filter
  if (is.null(par_list$ma_period)) {
    smaFilter <- price == price
  } else {
    smaTable <- xts::xts(apply(price, 2, TTR::SMA, par_list$ma_period), order.by = index(price))
    smaFilter <- price > smaTable
  }

  # absolute momentum filter
  absmaFilter <- momScore > 0  * smaFilter[index(momScore)]

  # rank filter
  rankFilter <- momRanking <= par_list$top_rank

  # select invest asset
  investAsset <- absmaFilter * rankFilter

  out <- list("momentum_score" = momScore,
              "invest_asset" = investAsset)

  return(out)
}

#' A bindModuleWeight Function
#' bind
#' @keywords say
#' @export
#' @param weight_list is list of asset parameters
#' @param parameter_set is xts object
#' @examples
#'#' bindModuleWeight(weight_list, parameter_set)
bindModuleWeight <- function (weight_list, parameter_set) {
  # parameter_set <- list(
  #
  #   "bond" = list("asset" = c("bond2"),
  #                 "top_rank" = 2,
  #                 "mom_period" = c(6),
  #                 "asset_weight" = c(0.2),
  #                 "ma_period" = NULL),
  #
  #   "global" = list("asset" = c("brics_stock2", "emerging_bond2", "emerging_stock2",
  #                               "global_bond2", "global_stock2", "index_stock2"),
  #                   "top_rank" = c(3),
  #                   "mom_period" = c(1:12),
  #                   "asset_weight" = c(0.8),
  #                   "ma_period" = 200)
  # )
  allAssets <- unlist(lapply(parameter_set, function (x) {x$asset}))
  duplicateAsset <- allAssets[duplicated(allAssets)]
  if (length(duplicateAsset) != 0) {
    uniqueAsset <- parameter_set$global$asset[-c(match(duplicateAsset, parameter_set$global$asset))]
    weight_list$bond <- weight_list$global[, duplicateAsset] + weight_list$bond[, duplicateAsset]
    weight_list$global <- weight_list$global[, uniqueAsset]
  }
  weightTable <- Reduce(cbind, weight_list)
  cashWeight <- 1 - rowSums(weightTable)

  allbindWeightTable <- cbind(weightTable, cashWeight)
  colnames(allbindWeightTable) <- c(colnames(weightTable), "CASH")
  return(allbindWeightTable)
}