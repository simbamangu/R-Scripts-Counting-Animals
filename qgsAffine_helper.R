## Compute correct affine numbers for qgsAffine plugin
affine <- function(originX, originY, rotAngle) {
  A <- rotAngle * pi / 180
  scaleX <- scaleY <- cos(A)
  rotX <- sin(A)
  rotY <- -sin(A)
  transX <- originX - cos(A) * originX + sin(A) * originY
  transY <- originY - sin(A) * originX - cos(A) * originY
  aff <- data.frame(scaleX, scaleY, rotX, rotY, transX, transY)
  return(aff)
}