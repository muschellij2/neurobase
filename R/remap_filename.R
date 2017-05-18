#' @title Build Filename (usually for images)
#' @description This is a simple function that helps with the case where you want to 
#' construct a filename (usually for an image) with the same base of the
#' filename, the same directory (default), but things added to the front or end
#' of that base filename, with the same extension.
#' @param x input filename/character vector
#' @param sub_dir sub-directory for the new filename.  If \code{NULL}, then 
#' the directory is the the same directory as \code{x}
#' @param prefix string to put in front of base of filename
#' @param suffix string to put at the end of base of filename
#'
#' @return Character vector
#' @export
#'
#' @examples
#' fname = file.path("/path/to/file", "original.nii.gz")
#' remap_filename(fname, prefix = "preproc_", "_with_gz")
#' fname = "original.nii"
#' remap_filename(fname, prefix = "note_", "_has_directory")
#' remap_filename(c(fname, "other.nii.gz"), prefix = "note_", "_has_directory")
remap_filename = function(
  x, 
  sub_dir = NULL,
  prefix = "", 
  suffix = "") {
  
  ext = parse_img_ext(x)
  ext = paste0(".", ext)
  tx = tolower(x)
  gz = grepl("gz$", tx)
  ext[gz] = paste0(ext[gz], ".gz") 
  dn = dirname(x)
  stub = nii.stub(x, bn = TRUE)
  
  if (!is.null(sub_dir)) {
    dn = file.path(dn, sub_dir)
  }
  file.path(
    dn,
    paste0(
      prefix, 
      stub,
      suffix,
      ext)
  )
}

