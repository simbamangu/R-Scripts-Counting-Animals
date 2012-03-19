rotatePoints <- function(shfile, rotation){
  rotshape <- readShapePoints(shfile)
  SP <- as(rotshape, "SpatialPoints")
  rotobj <- as(SP, "ppp")

  xvec <- rotobj$x
  yvec <- rotobj$y
	xmin <- min(xvec)
	ymin <- min(yvec)
  xmax <- max(xvec)
  ymax <- max(yvec)
	xvec <- xvec - xmin - (ymax - ymin) / 2
	yvec <- yvec - ymin - (xmax - xmin) / 2
	rotR <- (rotation * pi) / 180
	curr.ppp <- ppp(xvec, yvec, window = owin(c(min(xvec), max(xvec)), c(min(yvec), max(yvec))))
	curr.ppp <- rotate.ppp(curr.ppp, rotR)

	return(data.frame(cbind(curr.ppp$x + xmin + (xmax - xmin)/2, curr.ppp$y + ymin + (ymax - ymin) / 2)))
  
	}