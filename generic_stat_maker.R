proper = function(x){
  paste0(toupper(substr(x, 1,1)), substr(x, 2, nchar(x)))
}

#############################################
# Function maker that involves 1 file
#############################################
makefunc = function(func,
                    remove = TRUE){
  
  x = readLines('generic_stat.R')
  x = gsub("%func%", func, x)
  x = gsub("%Func%", proper(func), x)

  fname = paste0("R/", func, ".nifti.R")
  writeLines(text = x, con = fname)
  if (remove) {
    file.remove(fname)
  }
  return(TRUE)
}



remove = FALSE
################################################################
# Make Functions that take in 1 file
################################################################
# makefunc("r", FUNC = "Robust Range", func = "rrange",
#          remove = TRUE)


makefunc("mean", remove = remove)
# makefunc("median", remove = remove)
# makefunc("sd", remove = remove)
# makefunc("var", remove = remove)






