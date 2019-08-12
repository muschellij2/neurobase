#' Subset DTI data based on b-values
#' #' 
#' @param img character or \code{nifti} object
#' @param bvals filename of b-values (assuming 1 row)
#' @param bvecs filename of b-vectors (assuming 3 rows)
#' @param b_step step of b-values to round to 
#' @param maximum maximum b-value threshold
#' @param shells Shells to keep (after rounding)
#' @param verbose print diagnostic messages
#' @param ... options passed to \code{\link{checkimg}}
#'
#' @return List of filenames of 
#' image, b-values, and b-vectors that were subsetted.
#' 
#' @export
#' 
#' @author John Muschelli \email{muschellij2@@gmail.com}
#' @name subset_dti-methods
#' @docType methods 
#' @aliases subset_dti
#' 
#'  
#' @examples \dontrun{
#' img = "~/Downloads/data.nii.gz"
#' bvals = "~/Downloads/bvals"
#' bvecs = "~/Downloads/bvals"
#' verbose = TRUE
#' b_step = 50
#' maximum = 1500
#' shells = NULL
#' sub = subset_dti(img = img, bvals = bvals, bvecs = bvecs, 
#' maximum = 1500,
#' b_step = 50)
#' 
#' }
#' @importFrom RNifti updateNifti
setGeneric("subset_dti", 
           function(
             img, 
             bvals, 
             bvecs, 
             b_step = 1, 
             maximum = Inf, 
             shells = NULL,
             verbose = TRUE,
             ...) standardGeneric("subset_dti"))


#' @rdname subset_dti-methods
#' @aliases subset_dti,nifti-method
#' @export
setMethod("subset_dti", "nifti",            
          function(
            img, 
            bvals, 
            bvecs, 
            b_step = 1, 
            maximum = Inf, 
            shells = NULL,
            verbose = TRUE,
            ...)  { 
            
            L = get_ind(
              bvals = bvals,
              bvecs = bvecs, 
              b_step = b_step,
              maximum = maximum,
              shells = shells,
              verbose = verbose)
            indices = L$indices
            img = img[,,,indices]
            img = checkimg(img, ...)
            L$img = img
            return(L)
          }) 

#' @rdname subset_dti-methods
#' @aliases subset_dti,nifti-method
#' @export
setMethod("subset_dti", "ANY",            
          function(
            img, 
            bvals, 
            bvecs, 
            b_step = 1, 
            maximum = Inf, 
            shells = NULL,
            verbose = TRUE,
            ...)  { 
            
            L = get_ind(
              bvals = bvals,
              bvecs = bvecs, 
              b_step = b_step,
              maximum = maximum,
              shells = shells,
              verbose = verbose)
            indices = L$indices
            is_niftiImage = inherits(img, "niftiImage")
            if (is_niftiImage) {
              vals = niftiHeader(img)
            }
            img = img[,,,indices]
            if (is_niftiImage) {
              img = RNifti::updateNifti(img, template = vals)
              rm(list = "vals"); gc(); gc()
            }
            img = checkimg(img, ...)
            L$img = img
            rm(list = "img"); gc(); gc()
            return(L)
          }) 


#' @rdname subset_dti-methods
#' @aliases subset_dti,character-method
#' @importFrom R.utils gunzip
#'  
#' @export
setMethod("subset_dti", "character",            
          function(
            img, 
            bvals, 
            bvecs, 
            b_step = 1, 
            maximum = Inf, 
            shells = NULL,
            verbose = TRUE,
            ...) { 
            ### add vector capability
            if (length(img) > 1) {
              ### add vector capability
              L = lapply(
                img, subset_dti, 
                bvals = bvals, 
                bvecs = bvecs, 
                b_step = b_step, 
                maximum = maximum, 
                shells = shells,
                verbose = verbose,
                ...)
              gc()
              return(L)
            } else {
              if (verbose) {
                message("Reading image using RNifti::readNifti")
              }
              img = RNifti::readNifti(img)
              L = subset_dti(
                img,
                bvals = bvals, 
                bvecs = bvecs, 
                b_step = b_step, 
                maximum = maximum, 
                shells = shells,
                verbose = verbose,
                ...)
              rm(list = "img"); gc()
              return(L)
            }
          })


#' @rdname subset_dti-methods
#' @aliases subset_dti,list-method
#' @export
setMethod("subset_dti", "list",            
          function(
            img, 
            bvals, 
            bvecs, 
            b_step = 1, 
            maximum = Inf, 
            shells = NULL,
            verbose = TRUE,
            ...) { 
            ### add vector capability
            L = lapply(
              img, subset_dti, 
              bvals = bvals, 
              bvecs = bvecs, 
              b_step = b_step, 
              maximum = maximum, 
              shells = shells,
              verbose = verbose,
              ...)
            return(L)
          })















############################
# non exported
############################
get_ind = function(
  bvals, bvecs, 
  b_step = 1,
  maximum = Inf, 
  shells = NULL,
  verbose = TRUE) {
  bvals = read_bvals(bvals)
  bvals = round(bvals / b_step) * b_step 
  if (verbose) {
    message(paste0("There are ", length(bvals), " b-values"))
    ub = sort(unique(bvals))
    message("The following are the unique shells (after rounding):")
    message(paste(ub, collapse = ", "))
  }
  bvecs = read_bvecs(bvecs)
  
  ind = bval_indices(
    bvals = bvals,
    maximum = maximum, 
    shells = shells)
  
  # write out bvecs
  bvecs = lapply(bvecs, function(x) x[ind])
  bvecs = sapply(bvecs, paste, collapse = " ")
  tfile = tempfile()
  bvecs = writeLines(bvecs, con = tfile)
  bvecs = tfile
  
  tfile = tempfile()
  bvals = bvals[ind]
  if (verbose) {
    ub = sort(unique(bvals))
    message("The following are the unique shells (after thresholding):")
    message(paste(ub, collapse = ", "))
  }  
  bvals = paste(bvals, collapse = " ")
  bvals = writeLines(bvals, con = tfile)
  bvals = tfile
  

  
  L = list(indices = ind,
           bvals = bvals,
           bvecs = bvecs)
  if (verbose) {
    message(paste0("There are ", length(ind), " shells to be kept"))
  }  
  return(L)
}

read_bvals = function(bvals) {
  bvals = readLines(bvals)
  bvals = strsplit(bvals, split = " ")[[1]]
  bvals = bvals[ !(bvals %in% "")]
  bvals = as.numeric(bvals)
  return(bvals)
}

read_bvecs = function(bvecs) {
  bvecs = readLines(bvecs)
  bvecs = strsplit(bvecs, split = " ")
  bvecs = lapply(bvecs, function(r) {
    r = r[ !(r %in% "")]
  })
  return(bvecs)
}

bval_indices = function(
  bvals,
  maximum = Inf, 
  shells = NULL,
  verbos) {
  
  keep = rep(TRUE, length = length(bvals))
  if (!is.null(shells)) {
    keep = keep & (bvals %in% shells)
  }
  keep = keep & (bvals <= maximum)
  
  ind =   which(keep)
  
  return(ind)
}
