## ----setup, include=FALSE------------------------------------------------
library(neurobase)
library(oro.nifti)
library(methods)
library(ggplot2)
library(httr)
library(reshape2)
knitr::opts_chunk$set(
  echo = TRUE,  comment = "")

## ---- eval = FALSE-------------------------------------------------------
#  packages = installed.packages()
#  packages = packages[, "Package"]
#  if (!"oro.nifti" %in% packages) {
#    install.packages("oro.nifti")
#    ### development version
#    # devtools::install_github("bjw34032/oro.nifti")
#  }

## ----nifti_obj-----------------------------------------------------------
library(oro.nifti)
set.seed(20161007)
dims = rep(10, 3)
arr = array(rnorm(10*10*10), dim = dims)
nim = oro.nifti::nifti(arr)
print(nim)
print(class(nim))
oro.nifti::is.nifti(nim)

## ----nifti_slot----------------------------------------------------------
nim@cal_max
cal_max(nim)
slot(nim, "cal_max")

## ----nifti_data----------------------------------------------------------
data = slot(nim, ".Data")
class(data)

## ----nifti_data2---------------------------------------------------------
data = oro.nifti::img_data(nim)
class(data)
dim(data)

## ----nifti_slice---------------------------------------------------------
slice = data[,,3]
class(slice)

## ----nifti_arr_slice-----------------------------------------------------
slice = data[,,3, drop = FALSE]
class(slice)

## ----slotnames-----------------------------------------------------------
slotNames(nim)

## ----logical-------------------------------------------------------------
above_zero = nim > 0
class(above_zero)
img_data(above_zero)[1]

## ----multi_log-----------------------------------------------------------
class(nim > 0 & nim < 2)

## ------------------------------------------------------------------------
class(nim * 2)
class(nim + (nim / 4))
class(nim * nim)
class(nim^2)

## ----sum-----------------------------------------------------------------
sum(above_zero)

## ----mean----------------------------------------------------------------
mean(above_zero)

## ----summs---------------------------------------------------------------
min(nim)
max(nim)
range(nim)
class(abs(nim))

## ----read_eve, cache = FALSE---------------------------------------------
eve_types = c("T1", "T2", "T1_Brain")
eve_stubs = paste0("JHU_MNI_SS_", eve_types, ".nii.gz")
url = "https://raw.githubusercontent.com/"
paths = paste(c("jfortin1",
                "EveTemplate", 
                "master",
                # "raw",
                "inst",
                "extdata"), 
              collapse = "/")
paths = paste(paths, eve_stubs, sep = "/")
path = paths[1]
eve_fnames = sapply(paths, function(path) {
  tmp = tempfile(fileext = ".nii.gz")
  req <- httr::GET(url, 
                   path = path, 
                   httr::write_disk(path = tmp),
                   httr::progress())
  httr::stop_for_status(req)
  return(tmp)
})
names(eve_fnames) = eve_types
readEve = function(what = c("T1", "T2", "Brain")) {
  what = match.arg(what)
  if (what == "Brain") {
    what = "T1_Brain"
  }
  fname = eve_fnames[what]
  readnii(fname)
}

## ----eve, cache=FALSE----------------------------------------------------
eve = readEve(what = "Brain")

## ----ortho---------------------------------------------------------------
oro.nifti::orthographic(eve)

## ----ortho2--------------------------------------------------------------
neurobase::ortho2(eve)

## ----ortho2_noorient-----------------------------------------------------
neurobase::ortho2(eve, add.orient = FALSE)

## ----ortho_nona----------------------------------------------------------
orthographic(eve, y = eve > quantile(eve, 0.9))

## ----ortho2_nona---------------------------------------------------------
ortho2(eve, y = eve > quantile(eve, 0.9))

## ----eve2, cache=FALSE---------------------------------------------------
eve2 = eve
eve2[which.max(eve)] = eve2[which.max(eve)] * 5

## ----ortho2_large--------------------------------------------------------
ortho2(eve2)

## ----ortho2_rob----------------------------------------------------------
ortho2(robust_window(eve2))

## ----ortho2_zlim---------------------------------------------------------
ortho2(eve2, zlim = quantile(eve2, probs = c(0, 0.999)))

## ----eve_full, cache = FALSE---------------------------------------------
eve_full = readEve(what = "T1")

## ----double_ortho--------------------------------------------------------
double_ortho(eve_full, eve)

