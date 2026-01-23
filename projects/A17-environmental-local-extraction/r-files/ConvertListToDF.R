#' Helper function to convert a List of similarly named dataframes into a long dataframe comprised
#' of all elements in the list. Used to combine BatchEnvAssign products

ConvertListToDF <- function(dataList) {
  dataLong <- data.frame(matrix(ncol = length(names(dataList[[1]][[1]])), nrow = 0))
  names(dataLong) <- names(dataList[[1]][[1]])
  for (i in 1:length(dataList)) {
    dataLong <- rbind(dataLong, dataList[[i]][[1]])
  }
  return(dataLong)
}