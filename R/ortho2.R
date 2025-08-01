#' @title Orthographic Display, added options
#' @description Copy of \code{oro.nifti}'s \code{\link[oro.nifti]{orthographic}} function 
#' with some tweaks such as adding L/R designations for left and right
#' @return NULL
#' @seealso \link[oro.nifti]{orthographic}
#' @param x is an object of class nifti or similar.
#' @param y is an object of class nifti or similar for the overlay.
#' @param xyz is the coordinate for the center of the crosshairs.
#' @param w is the time point to be displayed (4D arrays only).
#' @param col is grayscale (by default).
#' @param col.y is hotmetal (by default).
#' @param zlim is the minimum and maximum `z' values passed into image.
#' @param zlim.y is the minimum and maximum `z' values passed into image 
#' for the overlay.
#' @param crosshairs is a logical value for the presence of crosshairs 
#' in all three orthogonal planes (default = TRUE).
#' @param NA.x Set any values of 0 in \code{x} to \code{NA}
#' @param NA.y Set any values of 0 in \code{y} to \code{NA}
#' @param col.crosshairs is the color of the crosshairs (default = red).
#' @param xlab is set to "" since all margins are set to zero.
#' @param ylab is set to "" since all margins are set to zero.
#' @param axes is set to FALSE since all margins are set to zero.
#' @param oma is the size of the outer margins in the par function.
#' @param mar is the number of lines of margin in the par function.
#' @param bg is the background color in the par function.
#' @param text allows the user to specify text to appear in 
#' the fourth (unused) pane.
#' @param text.color is the color of the user-specified text 
#' (default = ``white").
#' @param text.cex is the size of the user-specified text (default = 2).
#' @param text.x x coordinate for text 
#' @param text.y y coordinate for text
#' @param add.orient (logical) Add left/right, A/P, etc. orientation
#' @param mfrow (numeric) layout of the 3 slices
#' @param breaks (numeric) breaks for x to passed to 
#' \code{\link[graphics]{image}}
#' @param ybreaks (numeric) breaks for y to passed to 
#' \code{\link[graphics]{image}}
#' @param addlegend (logical) add legend?
#' @param leg.x (numeric) x coordinate for legend
#' @param leg.y (numeric) y coordinate for legend
#' @param legend (character) legend text
#' @param leg.col (character) Colors for legend 
#' @param leg.title (character) title for legend 
#' @param leg.cex (numeric) \code{cex} for \code{\link{legend}}
#' @param window (vector) Length-2 vector to limit image to certain range
#' @param ycolorbar (logical) Should a colorbar for \code{y} be plotted
#' @param clabels Label for colorbar (see \code{\link{colorbar}})
#' @param add Should the y-plot be added or its own plot?  Used
#' in \code{double_ortho}
#' @param pdim Pixel dimensions if passing in arrays.  Will be overridden if 
#' \code{x} is a \code{nifti} object
#' @param useRaster logical; if TRUE a bitmap raster is used to 
#' plot the image instead of polygons.  Passed to 
#' \code{\link[graphics]{image}}.
#' @param mask If a mask is passed, \code{drop_empty_dim} is applied 
#' to both \code{x} and \code{y}
#' @param ... other arguments to the image function may be provided here.
#' @export
#' @import graphics
#' @importFrom grDevices col2rgb gray rgb
#' @examples 
#' set.seed(10)
#' x = oro.nifti::nifti(array(rnorm(1000), dim = rep(10, 3)))
#' ortho2(x)
#' y = x > 2
#' mask = x > 2.5
#' ortho2(x, y)
#' ortho2(x, y, mask = mask, add.orient = TRUE)
#' ortho2(x, y, mask = mask, add.orient = TRUE, add = FALSE)
#' nim = RNifti::asNifti(x, internal = FALSE)
#' ortho2(nim, y, mask = mask)
#' neurobase::ortho2(nim, x, mask = mask, 
#' ybreaks = seq(min(x), max(x), length.out = 65), ycolorbar = TRUE)
#' 
#' ortho2(nim, y, mask = mask, add = FALSE)
#' arr_x = as.array(x)
#' arr_y = as.array(y)
#' ortho2( arr_x)
#' ortho2( arr_x, arr_y, useRaster = FALSE)
#' 
#' set.seed(10)
#' x = oro.nifti::nifti(array(rnorm(10000), dim = rep(10, 4)))
#' y = x > 2
#' mask = x > 2.5
#' ortho2(x, y)
#' 
#' set.seed(10)
#' x = oro.nifti::nifti(array(rnorm(100), dim = rep(10, 2)))
#' y = x > 2
#' mask = x > 2.5
#' ortho2(x, y)
ortho2 = function(x, y = NULL, xyz = NULL, w = 1, col = gray(0:64/64), 
                  col.y = oro.nifti::hotmetal(), zlim = NULL, zlim.y = NULL, 
                  NA.x = FALSE,
                  NA.y = TRUE,
                  crosshairs = TRUE, 
                  col.crosshairs = "red", xlab = "", ylab = "", axes = FALSE, 
                  oma = c(0, 0, 0, ifelse(ycolorbar, 5, 0)), 
                  mar = rep(0, 4), bg = "black", text = NULL, 
                  text.color = "white", text.cex = 2, 
                  text.x=32,
                  text.y=32,
                  add.orient=FALSE,
                  mfrow=c(2,2), ybreaks = NULL, breaks=NULL,
                  addlegend = FALSE,
                  leg.x=32,
                  leg.y=32,
                  legend,
                  leg.col,
                  leg.title = NULL,
                  leg.cex,
                  window=NULL,
                  ycolorbar = FALSE,
                  clabels = TRUE,
                  add = TRUE,
                  pdim = NULL,
                  useRaster = is.null(y),
                  mask = NULL,
                  ...) 
{
  if (!is.null(mask)) {
    mask = check_nifti(mask, allow.array = TRUE)
    dd = dropEmptyImageDimensions(mask, keep_ind = TRUE)
    keep_inds = dd$inds
    rm(list = "dd");
  }
  x = check_nifti(x, allow.array = TRUE)
  ndim = length(dim(x))
  if (!is.null(y)) {
    if (!all(dim(x)[1:ndim] == dim(y)[1:ndim])) {
      stop("dimensions of \"x\" and \"y\" must be equal")
    }
  }
  x_is_nifti = FALSE
  if (inherits(x, "nifti")) {
    x_is_nifti = TRUE
    if (!is.null(mask)) {
      x = apply_empty_dim(img = x, inds = keep_inds)
    }
  } else {
    if (!is.null(mask)) {
      x = x[keep_inds[[1]], keep_inds[[2]], keep_inds[[3]]]    
    }
  }
  if (!is.null(window)) {
    x = window_img(x, window = window, replace = "window")
  }
  
  X <- nrow(x)
  Y <- ncol(x)
  Z <- nsli(x)
  if (all(is.na(Z))) Z = 1
  W <- ntim(x)
  # mXY = max(X, Y)
  lr.shift = 4
  ud.shift = 6
  range_y = NULL
  if (!is.null(y)) {
    y = check_nifti(y, allow.array = TRUE)
    y_is_nifti = FALSE
    if (inherits(y, "nifti")) {
      y_is_nifti = TRUE  
      if (!is.null(mask)) {
        y = apply_empty_dim(img = y, inds = keep_inds)
      }
    } else {
      if (!is.null(mask)) {
        y = y[keep_inds[[1]], keep_inds[[2]], keep_inds[[3]]]
      }
    }
    if (NA.y) {
      y[ y == 0 ] = NA
      if (all(is.na(y))) {
        stop(paste0("y has no non-zero values and NA.y = TRUE.  ", 
                    "Either remove the overlay, or set NA.y = FALSE"))
      }
    }
    if (y_is_nifti) {
      y = cal_img(y)
      range_y = c(cal.min(y), cal.max(y))
    } else {
      range_y = range(y, na.rm = TRUE)
    }
  }
  if (NA.x) {
    x[ x == 0 ] = NA
  }
  if (x_is_nifti) {
    x = cal_img(x)
    range_x = c(cal.max(x), cal.min(x))
  } else {
    range_x = range(x, na.rm = TRUE)
  }
  if (is.null(xyz)) {
    xyz <- ceiling(c(X, Y, Z)/2)
  }
  if (X == 0 || Y == 0 || Z == 0) {
    stop("size of NIfTI volume is zero, nothing to plot")
  }  
  zlim = zlimmer(x, zlim = zlim, computed_range = range_x)
  
  if (is.null(breaks)) {
    range_x_zlim = range(c(range_x, zlim), na.rm = TRUE)
    breaks <- c(range_x_zlim[1],
                seq(min(zlim, na.rm = TRUE),
                    max(zlim, na.rm = TRUE), 
                    length = length(col) - 1),
                range_x_zlim[2])
  }
  
  zlim.y = zlimmer(y, zlim = zlim.y, computed_range = range_y)
  
  oldpar <- par(no.readonly = TRUE)
  par(mfrow = mfrow, oma = oma, mar = mar, bg = bg)
  
  if (x_is_nifti) {
    pdim = pixdim(x)
  } else {
    if (inherits(x, "niftiImage")) {
      if (is.null(pdim)) {
        pdim = c(1, pixdim(x))
        if (length(pdim) <= 3) {
          pdim = c(pdim, rep(1, 4 - length(pdim)))
        }
      }
    }
    if (is.null(pdim)) {
      pdim = rep(1, 4)
    } 
  }
  stopifnot(length(pdim) >= 4)
  
  if (!all(is.na(W))) {
    if (any(w < 1 || w > W)) {
      stop("volume \"w\" out of range")
    }
    x = x[, , , w]
    if (!is.null(y)) {
      if (inherits(y, "nifti") || inherits(y, "anlz")) {
        # class(y@.Data) == "numeric"
        y = as.array(y)
      }      
      y = y[, , , w]
    }
  }
  if (Z == 1) {
    x = abind::abind(x, x, along = 3)
    if (!is.null(y)) {
      y = abind::abind(y, y, along = 3)
    }
    Z = 2
  }
  if (X == 1) {
    x = abind::abind(x, x, along = 1)
    if (!is.null(y)) {
      y = abind::abind(y, y, along = 1)
    }
    X = 2
  } 
  if (Y == 1) {
    x = abind::abind(x, x, along = 2)
    if (!is.null(y)) {
      y = abind::abind(y, y, along = 2)
    }
    Y = 2
  }   
  
  # L = list(X = 1:X,
  #          Y = 1:Y,
  #          Z = 1:Z)
  # grapher = function(img, idim, zlimit, adder, colors, runbreaks, ...){
  #   xyz_val = xyz[idim]
  #   if (idim == 1) {
  #     vec = img[xyz_val, , ]
  #   }
  #   if (idim == 2) {
  #     vec = img[, xyz_val, ]
  #   }
  #   if (idim == 3) {
  #     vec = img[, , xyz_val]
  #   }
  #   dims = 1:3
  #   grab = !(dims %in% idim)
  #   l = L[grab]
  #   asp = (dims + 1)[grab]
  #   asp = pdim[asp[2]]/pdim[asp[1]]
  #   
  #   graphics::image(l[[1]], l[[2]], vec, col = colors, 
  #                   zlim = zlimit,
  #                   add = adder,
  #                   breaks = runbreaks, 
  #                   asp = asp, xlab = ylab, 
  #                   ylab = xlab, axes = axes, useRaster = useRaster, 
  # ...)
  # }
  # grapher(img = x, idim = 2, zlimit = zlim, adder = FALSE, colors = col, 
  #         runbreaks = breaks, ...)
  graphics::image(1:X, 1:Z, as.matrix(x[, xyz[2], ]), col = col, zlim = zlim, 
                  breaks = breaks, asp = pdim[4]/pdim[2], xlab = ylab, 
                  ylab = xlab, axes = axes, 
                  useRaster = useRaster, ...)
  if (!add & crosshairs) {
    abline(h = xyz[3], v = xyz[1], col = col.crosshairs)
  }
  if (!add & add.orient) {
    text("L", x = X + lr.shift, y = Z/2, las = 1, col = "white")
    text("R", x = -lr.shift, y = Z/2, las = 1, col = "white")
    text("S", x = X/2 - .5, y = Z - ud.shift, las = 1, col = "white")
    text("I", x = X/2 - .5, y = ud.shift, las = 1, col = "white")
  }  
  if (!is.null(y)) {
    if (is.null(ybreaks)) {
      graphics::image(1:X, 1:Z, as.matrix(y[, xyz[2], ]), col = col.y, 
                      zlim = zlim.y, add = add,
                      asp = ifelse(add, NA, pdim[4]/pdim[2]),
                      axes = axes,
                      useRaster = useRaster
      )
    } else {
      graphics::image(1:X, 1:Z, as.matrix(y[, xyz[2], ]), col = col.y, 
                      zlim = zlim.y, add = add, breaks = ybreaks,
                      asp = ifelse(add, NA, pdim[4]/pdim[2]),
                      axes = axes,
                      useRaster = useRaster
      )
    }
  }
  if (crosshairs) {
    abline(h = xyz[3], v = xyz[1], col = col.crosshairs)
  }
  if (add.orient) {
    text("L", x = X + lr.shift, y = Z/2, las = 1, col = "white")
    text("R", x = -lr.shift, y = Z/2, las = 1, col = "white")
    text("S", x = X/2 - .5, y = Z - ud.shift, las = 1, col = "white")
    text("I", x = X/2 - .5, y = ud.shift, las = 1, col = "white")
  }
  graphics::image(1:Y, 1:Z, as.matrix(x[xyz[1], , ]), col = col, breaks = breaks, 
                  asp = pdim[4]/pdim[3], xlab = xlab, ylab = ylab, 
                  axes = axes,
                  useRaster = useRaster, ...)
  if (!add & crosshairs) {
    abline(h = xyz[3], v = xyz[2], col = col.crosshairs)
  }
  if (!add & add.orient) {
    text("A", x = Y - 1, y = Z/2, las = 1, col = "white")
    text("P", x = 0 + 1, y = Z/2, las = 1, col = "white")
    text("S", x = Y/2 - .5, y = Z - ud.shift, las = 1, col = "white")
    text("I", x = Y/2 - .5, y = ud.shift, las = 1, col = "white")
  }  
  if (!is.null(y)) {
    if (is.null(ybreaks)) {
      graphics::image(1:Y, 1:Z, as.matrix(y[xyz[1], , ]), col = col.y, 
                      zlim = zlim.y, add = add,
                      asp = ifelse(add, NA, pdim[4]/pdim[3]),
                      axes = axes,
                      useRaster = useRaster
      )
    } else {
      graphics::image(1:Y, 1:Z, as.matrix(y[xyz[1], , ]), col = col.y, 
                      zlim = zlim.y, add = add, breaks = ybreaks,
                      asp = ifelse(add, NA, pdim[4]/pdim[3]),
                      axes = axes,
                      useRaster = useRaster
      )
    }
  }
  if (crosshairs) {
    abline(h = xyz[3], v = xyz[2], col = col.crosshairs)
  }
  if (add.orient) {
    text("A", x = Y - 1, y = Z/2, las = 1, col = "white")
    text("P", x = 0 + 1, y = Z/2, las = 1, col = "white")
    text("S", x = Y/2 - .5, y = Z - ud.shift, las = 1, col = "white")
    text("I", x = Y/2 - .5, y = ud.shift, las = 1, col = "white")
  }    
  graphics::image(1:X, 1:Y, as.matrix(x[, , xyz[3]]), col = col, breaks = breaks, 
                  asp = pdim[3]/pdim[2], xlab = xlab, ylab = ylab, 
                  axes = axes,
                  useRaster = useRaster,
                  ...)
  if (!add & crosshairs) {
    abline(h = xyz[2], v = xyz[1], col = col.crosshairs)
  }
  if (!add & add.orient) {
    text("L", x = X + lr.shift, y = Y/2, las = 1, col = "white")
    text("R", x = -lr.shift, y = Y/2, las = 1, col = "white")
    text("A", x = X/2 - .5, y = Y - ud.shift, las = 1, col = "white")
    text("P", x = X/2 - .5, y = ud.shift, las = 1, col = "white")
  }    
  if (!is.null(y)) {
    if (is.null(ybreaks)) {
      graphics::image(1:X, 1:Y, as.matrix(y[, , xyz[3]]), col = col.y, 
                      zlim = zlim.y, add = add,
                      asp = ifelse(add, NA, pdim[3]/pdim[2]),
                      axes = axes,
                      useRaster = useRaster
      )
    } else {
      graphics::image(1:X, 1:Y, as.matrix(y[, , xyz[3]]), col = col.y, 
                      zlim = zlim.y, add = add, breaks = ybreaks,
                      asp = ifelse(add, NA, pdim[3]/pdim[2]),
                      axes = axes,
                      useRaster = useRaster
      )
    }
  }
  if (crosshairs) {
    abline(h = xyz[2], v = xyz[1], col = col.crosshairs)
  }
  if (add.orient) {
    text("L", x = X + lr.shift, y = Y/2, las = 1, col = "white")
    text("R", x = -lr.shift, y = Y/2, las = 1, col = "white")
    text("A", x = X/2 - .5, y = Y - ud.shift, las = 1, col = "white")
    text("P", x = X/2 - .5, y = ud.shift, las = 1, col = "white")
  }    
  
  if (!is.null(text) | addlegend) {
    suppressWarnings({
      graphics::image(1:64, 1:64, matrix(NA, 64, 64), xlab = "", 
                      ylab = "", axes = FALSE,
                      useRaster = useRaster,
                      ...)
    })
    if (addlegend) {
      legend(x = leg.x, y = leg.y, 
             legend = legend, 
             pch = rep(15, length(leg.col)),
             col = leg.col, 
             cex = leg.cex, 
             text.col = "white",
             title = leg.title)    
    }
    if (!is.null(text)) {
      text(labels = text, x = text.x, y = text.y, col = 
             text.color, cex = text.cex)
    }    
  }
  
  par(oldpar)
  if (!is.null(y)) {
    if (is.null(ybreaks)) {
      if (ycolorbar) {
        warning("colorbar not supported if ybreaks unspecified")
        #         nc <- length(col.y)
        #         if (diff(zlim.y) == 0) {
        #           zlim.y <- ifelse(zlim.y[1L] == 0, c(-1, 1), 
        #                 zlim.y[1L] + c(-0.4, 0.4) * abs(zlim.y[1L]))
        #         }
        #         zi = floor((nc - 1e-05) * y[, , xyz[3]] + 1e-07)
        #         breaks = unique(zi[zi >= 0 & zi < nc])
        #         
        #         colorbar(breaks=ybreaks, col=col.y, text.col="white")
      }
    } else {
      if (ycolorbar) {
        alpha = function(col, alpha = 1) {
          cols = t(col2rgb(col, alpha = FALSE)/255)
          rgb(cols, alpha = alpha)
        }        
        colorbar(breaks = ybreaks, col = alpha(col.y, 1), 
                 text.col = "white", 
                 labels = clabels)
      }
    }
  }
  invisible()
}


#' @title Add a colorbar to an ortho2 object
#'
#' @description Adds a series of colors mapped to a value
#' @param breaks a set of finite numeric breakpoints for the 
#' colours (see \code{\link{image}}
#' @param col a list of colors (see \code{\link{image}}
#' @param text.col axis and text label color
#' @param labels labels for tick marks - see \code{\link{axis}}
#' @param maxleft Extent the left hand for colorbar
#' @note Much of this was taken from \code{vertical.image.legend} from
#' the \code{aqfig} package
#' @import graphics
#' @export
#' @return A plot
colorbar <- function(breaks, #the minimum and maximum z values for which 
                     # colors should be plotted (see \code{\link{image}})
                     col, # a list of colors (see \code{\link{image}})
                     text.col = "white", # axis and text label color
                     labels = TRUE,
                     maxleft = 0.95
){
  # taken from vertical.image.legend from package aqfig
  starting.par.settings <- par(no.readonly = TRUE)
  on.exit({
    par(starting.par.settings)
  })
  mai <- par("mai")
  fin <- par("fin")
  rat = mai[4]/fin[1]
  rat = max(rat, 1 - maxleft)
  x.legend.fig <- c(1 - rat, 1)
  y.legend.fig <- c(mai[1]/fin[2], 1 - (mai[3]/fin[2]))
  x.legend.plt <- c(x.legend.fig[1] + (0.08 * (x.legend.fig[2] - 
                                                 x.legend.fig[1])), 
                    x.legend.fig[2] - (0.6 * (x.legend.fig[2] - 
                                                x.legend.fig[1])))
  y.legend.plt <- y.legend.fig
  cut.pts <- breaks
  z <- (cut.pts[1:length(col)] + cut.pts[2:(length(col) + 1)])/2
  par(new = TRUE, pty = "m", plt = c(x.legend.plt, y.legend.plt))
  image(x = 1, y = z, z = matrix(z, nrow = 1, ncol = length(col)), 
        col = col, xlab = "", ylab = "", xaxt = "n", yaxt = "n")
  if (isTRUE(labels)) {
    at = NULL
  } else {
    at = z
  }
  axis(4, mgp = c(3, 0.2, 0), las = 2, cex.axis = 0.5, 
       tcl = -0.1, 
       labels = labels,
       at = at,
       col.axis = text.col,
       col = text.col)
  box()
  mfg.settings <- par()$mfg
  par(mfg = mfg.settings, new = FALSE)
  invisible(NULL)
}
