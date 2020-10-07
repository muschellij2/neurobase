#' @title Image Rescaler
#' @name rescale_img
#' @param filename filename of image to be read into R or nifti object 
#' @param pngname filename of png of histogram of values of image to be made. For no
#' png - set to NULL (default)
#' @param write.nii logical - should the image be written.  
#' @param outfile if \code{write.nii = TRUE}, filename of output file
#' @param min.val minimum value of image (default -1024 (for CT)).  If no thresholding
#' set to -Inf
#' @param max.val maximum value of image (default 3071 (for CT)).  If no thresholding
#' set to Inf
#' @param ROIformat if TRUE, any values $< 0$ will be set to 0
#' @param writer character value to add to description slot of NIfTI header
#' @param drop_dim Should \code{\link{drop_img_dim}} be applied?
#' @param ... extra methods to be passed to \code{\link{writenii}}
#' @description Rescales an image to be in certain value range.  This was created
#' as sometimes DICOM scale and slope parameters may be inconsistent across sites
#' and the data need to be value restricted
#' @return Object of class nifti
#' @importFrom grDevices png dev.off
#' @export
#' @examples 
#' img = nifti(array(rnorm(10^3, sd = 1000), dim = rep(10, 3)))
#' outfile = tempfile(fileext = ".nii.gz")
#' pngname = tempfile(fileext = ".png")
#' rescale_img(img, write.nii = TRUE, outfile = outfile,
#' pngname = pngname)
rescale_img = function(filename, 
                       pngname = NULL, 
                       write.nii = FALSE,
                       outfile = NULL,
                       min.val = -1024,
                       max.val = 3071,
                       ROIformat=FALSE, 
                       drop_dim = TRUE,
                       writer = "dcm2nii", ...){
  
  if (write.nii){
    stopifnot(!is.null(outfile))
  }
  
  img = check_nifti(filename)
  # inter = as.numeric(img@scl_inter)
  # slope = as.numeric(img@scl_slope)
  # img = (img * slope + inter)
  r = range(c(img))
  if (!(r[1] >= min.val & r[2] <= max.val)) {
    img[img < min.val] = min.val
    img[img > max.val] = max.val
  }
  
  img = zero_trans(img)
  if (ROIformat) {
    img[img < 0] = 0
  }
  img = cal_img(img)
  descrip(img) = paste0("written by ", writer, " - ", descrip(img))
  if (drop_dim) {
    img = drop_img_dim(img)
  }
  #### create histograms
  if (!is.null(pngname)){
    options(bitmapType = 'cairo') 
    print(pngname)
    ### remove random percents
    pngname = gsub("%", "", pngname)
    grDevices::png(pngname)
    graphics::hist(img)
    grDevices::dev.off()
  }
  
  if (write.nii) {
    writenii(img, filename = outfile, drop_dim = drop_dim, ...)
  }
  return(img)
}




#' @title Change Data type for img
#' @return object of type nifti
#' @param img nifti object (or character of filename)
#' @param type_string (NULL) character of datatype and bitpix.  Supercedes
#' both datatype and bitpix.  If specified 
#' \code{convert.datatype[[type_string]]} and 
#' \code{convert.bitpix[[type_string]]} will be used.
#' @param datatype (NULL) character of datatype see 
#' \code{\link{convert.datatype}}
#' @param bitpix (NULL) character of bitpix see 
#' \code{\link{convert.bitpix}} 
#' @param trybyte (logical) Should you try to make a byte (UINT8) if image in
#' c(0, 1)?
#' @param warn Should a warning be issued if defaulting to FLOAT32?
#' @description Tries to figure out the correct datatype for image.  Useful 
#' for image masks - makes them binary if
#' @name datatype
#' @export
#' @examples 
#' img = nifti(array(rnorm(10^3, sd = 1000), dim = rep(10, 3)))
#' rimg = round(img)
#' newnii(datatyper(rimg))
#' rimg = datatyper(rimg, type_string= "FLOAT32")
datatyper = function(img, type_string = NULL,
                     datatype = NULL, bitpix=NULL, trybyte=TRUE,
                     warn = TRUE){
  img = check_nifti(img)
  if (!is.null(type_string)) {
    accepted = names(convert.datatype())
    type_string = toupper(type_string)
    stopifnot(type_string %in% accepted)
    datatype = convert.datatype()[[type_string]]
    bitpix = convert.bitpix()[[type_string]]
  }  
  if (!is.null(datatype) & !is.null(bitpix)) {
    datatype(img) <- datatype
    bitpix(img) <- bitpix
    return(img)
  }
  if (!is.null(datatype) & is.null(bitpix)) {
    stop("Both bitpix and datatype need to be specified if one is")
  }
  if (is.null(datatype) & !is.null(bitpix)) {
    stop("Both bitpix and datatype need to be specified if one is")
  }
  #### logical - sign to unsigned int 8
  arr = as(img, "array")
  is.log = inherits(arr[1], "logical")
  
  any_na = anyNA(arr)
  if (any_na) {
    warn_them = img@datatype < 64L
    img@"datatype" = max(64L, img@datatype)
    warn_them = warn_them | img@bitpix < 64L
    img@bitpix = max(64L, img@bitpix)
    if (warn_them) {
      warning("Need to change bitpix and datatype to FLOAT64 due to NAs")
    }
    return(img)
  }  
  if (is.log) {
    datatype(img) <- convert.datatype()$UINT8
    bitpix(img) <- convert.bitpix()$UINT8
    return(img)
  }
  #### testing for integers
  testInteger <- function(img){
    x = c(as(img, "array"))
    test <- all.equal(x, as.integer(x), check.attributes = FALSE)
    return(isTRUE(test))
  }  
  is.int = testInteger(img)
  if (is.int) {
    rr = range(img, na.rm = TRUE)
    ##### does this just for binary mask
    if (all(rr == c(0, 1)) & trybyte) {
      if (all(img %in% c(0, 1))) {
        datatype(img) <- convert.datatype()$UINT8
        bitpix(img) <- convert.bitpix()$UINT8
        return(img)
      }
    }
    signed = FALSE
    if (any(rr < 0)) {
      signed = TRUE
    }
    trange = diff(rr)
    # u = "U"
    mystr = NULL
    num = 16 # default is signed short
    if (is.null(mystr) & trange <= (2 ^ num) - 1 ) {
      # mystr = ifelse(signed, "INT16", "FLOAT32")
      mystr = "INT16"
    }
    
    num = 32 
    if (is.null(mystr) & trange <= (2 ^ num) - 1 ) {
      mystr = "INT32" # no UINT32 allowed
    }
    
    num = 64
    if (is.null(mystr) & trange <= (2 ^ num) - 1 ) {
      mystr = "FLOAT64" # Only way to 64 bits is through double
    }
    if (is.null(mystr)) {
      stop(paste0("Cannot determine integer datatype, ", 
                  "may want to recheck data or not use datatyper!"))
    }
    datatype(img) <- oro.nifti::convert.datatype()[[mystr]]
    bitpix(img) <-  oro.nifti::convert.bitpix()[[mystr]]
    return(img)
  } else {
    if (warn) {
      warning("Assuming FLOAT32 - changing @bitpix and @datatype in header")
    }
    mystr = "FLOAT32"
    datatype(img) <- convert.datatype()[[mystr]]
    bitpix(img) <- convert.bitpix()[[mystr]]
    return(img)
  }
}



#' @title Resets image parameters for a copied nifti object
#' @return object of type nifti
#' @param img nifti object (or character of filename)
#' @param ... arguments to be passed to \code{\link{datatype}}
#' @description Resets the slots of a nifti object, usually because an image
#' was loaded, then copied and filled in with new data instead of making a 
#' nifti object from scratch.  Just a wrapper for smaller functions 
#' @export
newnii = function(img, ...){
  img = check_nifti(img)
  img = zero_trans(img)
  img = cal_img(img)
  img = datatyper(img, ...)
  return(img)
}

