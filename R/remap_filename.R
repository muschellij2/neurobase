#' @title Build Filename (usually for images)
#' @description This is a simple function that helps with the case where you want to 
#' construct a filename (usually for an image) with the same base of the
#' filename, the same directory, but things added to the front or end
#' of that base filename, with the same extension.
#' @param x input filename/character vector
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
remap_filename = function(
  x, 
  prefix = "", 
  suffix = "") {
  
  ext = parse_img_ext(x)
  ext = paste0(".", ext)
  tx = tolower(x)
  if (grepl("gz$", tx)) {
    ext = paste0(ext, ".gz")
  }
  dn = dirname(x)
  stub = nii.stub(x, bn = TRUE)
  
  file.path(
    dn,
    paste0(
      prefix, 
      stub,
      suffix,
      ext)
  )
}

