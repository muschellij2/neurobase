#' @title Plot differences for Prediction and Gold Standard
#'
#' @description Uses \code{\link{ortho2}} to plot differences between a predicted binary
#' image and the assumed ground truth (\code{roi}).
#' @param img image to be underlaid
#' @param pred binary segmentation (prediction)
#' @param roi binary manual segmentation (ground truth)
#' @param xyz coordinate for the center of the crosshairs.
#' @param cols colors for false negatives/positives
#' @param levels labels for false negatives/positives
#' @param addlegend add legend, passed to \code{\link{ortho2}}
#' @param center run \code{\link{xyz}} on \code{roi}.  Disregarded if \code{xyz} is
#' not \code{NULL}
#' @param leg.cex multiplier for legend size
#' @param ... arguments to be passed to \code{\link{ortho2}} or
#' \code{\link{multi_overlay}}
#' @export
#' @seealso \code{\link{ortho2}}
#' @return NULL
#' @examples 
#' 
#' set.seed(5)
#' dims = rep(10, 3)
#' arr = array(rpois(prod(dims), lambda = 2), dim = dims)
#' nim = oro.nifti::nifti(arr)
#' roi = nim > 2
#' pred = nim > 1.5
#' ortho_diff(nim, pred, roi)
ortho_diff <- function(img, 
                       pred, # binary segmentation (prediction)
                       roi, # binary manual segmentation (ground truth)
                       xyz = NULL, 
                       cols = c("#56B4E9", "#D55E00", "#009E73"), # colors for false negatives, positives
                       levels = c("False Negative", "False Positive", "True Positive"), # labels for false negatives, positives
                       addlegend = TRUE, # add legend
                       center = TRUE, # run \code{\link{xyz}} on \code{roi} 
                       leg.cex = 1.5,  # multiplier for legend size
                       ...
){
  
  roi = check_nifti(roi, allow.array = TRUE)
  pred = check_nifti(pred, allow.array = TRUE)
  
  ###########################
  ### Drop empty image dimensions
  ###########################
  if (center) {
    if (is.null(xyz)){
      xyz = xyz(roi)
    } else {
      warning("xyz set, and center = TRUE, using user set xyz")
    }
  } else {
    xyz = xyz
  }
  
  # check_mask_fail(roi)
  # check_mask_fail(pred)
  
  pred = pred > 0
  roi = roi > 0
  
  diff = pred * NA
  # false negative
  diff[ as.integer(roi) %in% 1 & as.integer(pred) %in% 0] = 1
  # false positive
  diff[ as.integer(roi) %in% 0 & as.integer(pred) %in% 1] = 2
  # true positive
  diff[ as.integer(roi) %in% 1 & as.integer(pred) %in% 1] = 3
  if (oro.nifti::is.nifti(diff)) {
    diff = oro.nifti::cal_img(diff)
  }
  
  ortho2(x = img, 
         y = diff, 
         # don't do alpha blending
         col.y = cols,
         xyz = xyz, 
         addlegend = addlegend,
         legend = levels, 
         leg.col = cols, 
         leg.cex = leg.cex,
         ybreaks = c(0, 1.1, 2.1, 3.1), 
         ...
  )
  return(invisible(NULL))
}


#' @rdname ortho_diff
#' @param z slice to display
#' @export
#' @examples 
#' set.seed(5)
#' dims = rep(10, 3)
#' arr = array(rnorm(prod(dims)), dim = dims)
#' nim = oro.nifti::nifti(arr)
#' mask = nim > 2
#' pred = nim > 1.5
#' multi_overlay_diff(nim, roi = mask, pred = pred)
#' 
#' \donttest{
#' 
#'  if (requireNamespace("brainR", quietly = TRUE)) {
#'    visits = 1:3
#'    y = paste0("Visit_", visits, ".nii.gz")
#'    y = system.file(y, package = "brainR")
#'    y = lapply(y, readnii)
#' 
#'    y = lapply(y, function(r){
#'      pixdim(r) = c(0, rep(1, 3), rep(0, 4))
#'      dropImageDimension(r)
#'    })
#' 
#'    x = system.file("MNI152_T1_1mm_brain.nii.gz", 
#'                  package = "brainR")
#'    x = readnii(x)
#'    mask = x > 0
#'    alpha = function(col, alpha = 1) {
#'        cols = t(col2rgb(col, alpha = FALSE)/255)
#'        rgb(cols, alpha = alpha)
#'    }
#'    roi = y[[2]]
#'    pred = y
#'    multi_overlay_diff(x, roi = roi, pred = pred)
#'    multi_overlay_diff(x, roi = roi, pred = pred, 
#'          mask = mask, 
#'          main = paste0("\n", "Visit ", visits),
#'          text = LETTERS[visits],
#'          text.x = 0.9,
#'          text.y = 0.1,
#'          text.cex = 3)
#'  }
#' }
#' @param x List of images of class \code{nifti} or character vector of filenames
multi_overlay_diff <- function(
  x,
  pred, # binary segmentation (prediction)
  roi, # binary manual segmentation (ground truth)
  z = NULL, 
  cols = c("#56B4E9", "#D55E00", "#009E73"), # colors for false negatives, positives
  ...
){
  
  roi = check_nifti(roi, allow.array = TRUE)
  pred = check_nifti(pred, allow.array = TRUE)
  
  ###########################
  ### Drop empty image dimensions
  ###########################
  if (is.null(z)) {
    z = xyz(roi)[3]
  } else {
    z = z
  }
  
  
  if (!is.list(pred)) {
    pred = list(pred)
  }
  pred = lapply(pred, function(r) {
    is_niftiImage = inherits(r, "niftiImage")
    out = r > 0
    if (is_niftiImage) {
      out = RNifti::asNifti(image = out, 
                            reference = r, 
                            internal = FALSE)
    }
    out
  })
  if (!is.list(x)) {
    x = lapply(seq_along(pred), function(r) x)
  } else {
    stopifnot(length(x) == length(pred))
  }
  
  roi = roi > 0
  roi = c(roi)
  
  diff_img = function(pred) {
    diff = pred * NA
    pred = c(pred)
    # false negative
    diff[ c(roi) %in% 1 & c(pred) %in% 0] = 1
    # false positive
    diff[ c(roi) %in% 0 & c(pred) %in% 1] = 2
    # true positive
    diff[ c(roi) %in% 1 & c(pred) %in% 1] = 3
    if (is.nifti(diff)) {
      diff = cal_img(diff)
    }
    diff
  }
  pred = lapply(pred, diff_img)
  
  multi_overlay(
    x = x, y = pred, 
    # don't do alpha blending
    col.y = cols,
    z = z, 
    ybreaks = c(0, 1.1, 2.1, 3.1), 
    ...
  )
  
  return(invisible(NULL))
}

