#' @title Parse Image Extensions
#' @description Get image extensions from a filename
#' @param file Character filename to parse
#' @return Extension of file
#' @export
#'
#' @importFrom tools file_ext file_path_sans_ext
#' @examples
#' parse_img_ext("blah.nii.gz")
#' parse_img_ext("blah.mnc")
#' parse_img_ext("blah.nii")
#' parse_img_ext("blah")
#' parse_img_ext("blah.img")
#' parse_img_ext("blah.hdr")
#' parse_img_ext("blah.hdr.gz")
parse_img_ext = function(file){
  file = tolower(file)
  file = basename(file)
  ext = tools::file_ext(file)
  if (length(ext) > 0) {
    gz = ext %in% "gz"
    if (any(gz)) {
      file[gz] = tools::file_path_sans_ext(file[gz])
      ext[gz] = tools::file_ext(file[gz])
    } 
  } else {
    ext = NA
  }
  empty = ext %in% ""
  ext[empty] = NA
  return(ext)
}
