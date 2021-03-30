#' @title Read NIfTI file reoriented to RPI
#' 
#' @description  This function calls the \code{\link{readnii}} function after
#' calling \code{\link{orient_rpi_file}} to force RPI orientation.
#' @param file file name of the NIfTI file. 
#' @param ... Arguments to pass to \code{\link{readnii}}
#' @param verbose print diagnostics, passed to \code{\link{orient_rpi_file}}
#' 
#' @note `read_rpi` uses `RNifti` to ensure the reading orientation
read_rpi <- function(file, ..., verbose = TRUE) {
  args = list(...)
  n_args = names(args)
  if ("fname" %in% n_args) {
    stop("fname cannot be specified in readrpi!")
  }
  
  L = orient_rpi_file(file = file, verbose = verbose)
  file = L$img
  nim = readnii(fname = file, ...)
  return(nim)
}
