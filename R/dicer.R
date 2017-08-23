#' @title Calculate Dice from a Table
#' @description Simple wrapper to calculate the Dice Coefficient/Similarity Index
#' from a table
#' @param tab table or matrix that is 2 by 2
#' @param verbose should the Dice be printed before returned?
#' @return Numeric scalar (one number)
#' @export
#' @examples
#' tab = matrix(c(1000, 20, 20, 400), ncol = 2)
#' dicer(tab)
dicer = function(tab, verbose = TRUE){
  dtab = dim(tab)
  stopifnot(all(dtab == c(2,2)))
  good = 2 * tab[2, 2]
  dice = good/(good + tab[1, 2] + tab[2, 1])
  if (verbose) {
    print(dice)
  }
  return(dice)
}