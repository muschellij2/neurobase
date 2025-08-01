% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rescale_img.R
\name{rescale_img}
\alias{rescale_img}
\title{Image Rescaler}
\usage{
rescale_img(
  filename,
  pngname = NULL,
  write.nii = FALSE,
  outfile = NULL,
  min.val = -1024,
  max.val = 3071,
  ROIformat = FALSE,
  drop_dim = TRUE,
  writer = "dcm2nii",
  ...
)
}
\arguments{
\item{filename}{filename of image to be read into R or nifti object}

\item{pngname}{filename of png of histogram of values of image to be made. For no
png - set to NULL (default)}

\item{write.nii}{logical - should the image be written.}

\item{outfile}{if \code{write.nii = TRUE}, filename of output file}

\item{min.val}{minimum value of image (default -1024 (for CT)).  If no thresholding
set to -Inf}

\item{max.val}{maximum value of image (default 3071 (for CT)).  If no thresholding
set to Inf}

\item{ROIformat}{if TRUE, any values $< 0$ will be set to 0}

\item{drop_dim}{Should \code{\link[oro.nifti]{drop_img_dim}} be applied?}

\item{writer}{character value to add to description slot of NIfTI header}

\item{...}{extra methods to be passed to \code{\link{writenii}}}
}
\value{
Object of class nifti
}
\description{
Rescales an image to be in certain value range.  This was created
as sometimes DICOM scale and slope parameters may be inconsistent across sites
and the data need to be value restricted
}
\examples{
img = nifti(array(rnorm(10^3, sd = 1000), dim = rep(10, 3)))
outfile = tempfile(fileext = ".nii.gz")
pngname = tempfile(fileext = ".png")
rescale_img(img, write.nii = TRUE, outfile = outfile,
pngname = pngname)
}
