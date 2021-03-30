#' Reorient an Image to RPI orientation
#' 
#' @param file Object of class \code{nifti} or character path
#' @param verbose print diagnostic messages
#' @return List of 3 elements
#' \itemize{
#' \item{\code{img}: }{Reoriented image of class \code{nifti}}
#' \item{\code{convention}: }{Convention (Neurological/Radiological) of original image}
#' \item{\code{orientation}: }{Original image orientations}
#' }
#' @export
#' @note `orient_rpi` and `orient_rpi_file` uses `RNifti` to ensure the 
#' reading orientation
#' @examples 
#' lr_fname = system.file( "nifti", "mniLR.nii.gz", package = "oro.nifti")
#' img = readnii(lr_fname)
#' 
#' rl_fname = system.file( "nifti", "mniRL.nii.gz", package = "oro.nifti")
#' rl_img = readnii(rl_fname)
#' stopifnot(all(rl_img[nrow(rl_img):1,,] == img))
#' 
#' reor = orient_rpi(rl_fname)
#' stopifnot(all(img == reor$img))
#' 
#' rev = reverse_orient_rpi(reor$img, convention = reor$convention,
#' orientation = reor$orientation)
#' stopifnot(all(rev == rl_img))
orient_rpi = function(file, verbose = TRUE){
  L = orient_rpi_file(file = file, verbose = verbose)
  L$img = check_nifti(L$img)
  return(L)
}

#' @export
#' @rdname orient_rpi
orient_rpi_file = function(file, verbose = TRUE){
  file = checkimg(file)
  img = RNifti::readNifti(file)
  sorient = RNifti::orientation(img)
  RNifti::orientation(img) = "LAS"
  outfile = tempfile(fileext = ".nii.gz")
  RNifti::writeNifti(img, file = outfile)
  L = list(img = outfile,
           orientation = sorient)
  return(L)
}


#' Reverse Reorientation an Image to RPI orientation
#' 
#' @param file Object of class \code{nifti} or character path
#' @param convention Convention of original image (usually from \code{\link{orient_rpi}})
#' @param orientation Vector of length 3 from original image 
#' (usually from \code{\link{orient_rpi}})
#' @param verbose print diagnostic messages
#' @return Object of class \code{nifti}
#' @export
#' @note `reverse_orient_rpi` and `reverse_orient_rpi_file` uses `RNifti` to ensure the 
#' reading orientation
#' @export
reverse_orient_rpi = function(
  file, 
  convention = c("NEUROLOGICAL", "RADIOLOGICAL"), 
  orientation, verbose = TRUE){
  img = reverse_orient_rpi_file(file = file, 
                                 orientation = orientation, 
                                 verbose = verbose)
  img = check_nifti(img)
  return(img)
}

#' @rdname reverse_orient_rpi
#' @export
reverse_orient_rpi_file = function(
  file, 
  convention = c("NEUROLOGICAL", "RADIOLOGICAL"), 
  orientation, 
  verbose = TRUE){
  
  file = checkimg(file)
  stopifnot(nchar(orientation) == 3)
  
  img = RNifti::readNifti(file)
  RNifti::orientation(img) = orientation
  outfile = tempfile(fileext = ".nii.gz")
  RNifti::writeNifti(img, file = outfile)
  
  return(outfile)
}

#' @export
#' @rdname orient_rpi
is_rpi_oriented = function(file, verbose = FALSE) {
  img = RNifti::asNifti(file)
  res = RNifti::orientation(img) == "LAS"
  return(res)
}