% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ensure_array.R
\name{ensure_array}
\alias{ensure_array}
\title{Ensure an array output}
\usage{
ensure_array(img)
}
\arguments{
\item{img}{Image object (\code{\link[oro.nifti]{nifti}} or \code{niftiImage})}
}
\value{
Array of same dimensions as image object
}
\description{
Forces an array output for manipulation and overall conversion
}
\examples{
set.seed(5)
dims = rep(10, 3)
arr = array(rnorm(prod(dims)), dim = dims)
arr[,,10] = 0
nim = oro.nifti::nifti(arr)
rnifti = RNifti::asNifti(nim)
timg = tempimg(nim)
limg = list(factor(timg), factor(timg))
func = ensure_array
func(arr)
func(nim)
func(rnifti)
func(timg)
func(limg[[1]])
}
