#' Apply the perpetual inventory method to a time series of GCFC
#'
#' @param gfcf A numeric vector
#' @param profile A data.frame with the following fields:
#' vintageId (unique id in ascending order) and values (vectors of equal length)
#'
#' @return a numeric vector
#' @export
#'
#' @examples
#' gfcf <- c(10, 20, 30, 40)
#' df <- purrr::by_row(data.frame(vintageId=1:4), ~1:4, .to="values")
#' pim(gfcf, df)
pim <- function(gfcf, profile){
  profile <- convertProfileToMatrix(profile)
  result <- as.vector(gfcf %*% profile)
  return(result)
}

#' Convert the given data.frame containing vectors to a matrix where each vector
#' is shifted forwards by it's id
#'
#' @param profileValues A data.frame with the following fields:
#' vintageId (unique id in ascending order) and values (vectors of equal length)
#'
#' @return a matrix
#'
#' @examples
#' df <- purrr::by_row(data.frame(vintageId=1:4), ~1:4, .to="values")
#' convertProfileToMatrix(df)
convertProfileToMatrix <- function(profileValues){
  # the profile for each vintage lives on it's row
  # we need to shift this profile by an amount equal to the age of the vintage
  # at the same time we need to ensure the resulting matrix is a _square_ matrix
  # three things can happen:
  # 1) there are more vintages than profile values. This we fix inside the function.
  # 2) there are less vintages than profile values. This we fix after the function.
  # 3) the number of vintages is equal to the number of profile values
  numVintages <- max(profileValues$vintageId)
  result <- purrr::map2(profileValues$values, profileValues$vintageId,
                        function(values, id){
                          # fill out on the right side
                          values <- c(values, rep(0, max(0, numVintages - length(values))))
                          numValuesToTake <- numVintages - id + 1
                          # fill out on the left side
                          values <- c(rep(0, id - 1), values[1:numValuesToTake])
                          return(values)
                        }) %>%
    do.call(rbind, .) # row bind all rows to get a matrix

  result <- result[, 1:numVintages]

  return(result)
}
