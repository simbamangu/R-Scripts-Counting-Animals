## Compute correct affine numbers for qgsAffine plugin
## from www.underdiverwaterman.com
## Released under a GPL license.
#
# Arguments:
#  originX, originY ⇒ coords of point aroudn which to rotate;
#  rotAngle ⇒ angle at which to rotate
# Returns:
#  data frame with required variables for affine.

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