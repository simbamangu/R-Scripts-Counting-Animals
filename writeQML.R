### Generating QML files for raster visibility in R
# 2015-07-14 below is minimal QML file needed for raster rendering 
# (by testing a qml file to destruction)

# <qgis>
#   <pipe>
#     <rasterrenderer band="1" type="singlebandpseudocolor">
#       <rastershader>
#         <colorrampshader colorRampType="INTERPOLATED" clip="0">
#           <item alpha="255" value="0" label="0.01" color="#fdcc8a"/>
#           <item alpha="255" value="6" label="6" color="#fc8d59"/>
#           <item alpha="255" value="16.3837" label="16.4" color="#d7301f"/>
#         </colorrampshader>
#       </rastershader>
#     </rasterrenderer>
#   </pipe>
# </qgis>

writeQML <- function(bpoints, bcolours=NA, fname=NA, brewcols = "YlOrBr") {
  # if (length(bpoints) != length(bcolours)) break
  require(XML)
  require(RColorBrewer)
  if (is.na(bcolours)) { 
    bcolours <- brewer.pal(length(bpoints)+1, brewcols) }
  qml <- newXMLNode(c('qgis'))
  newXMLNode("pipe", parent=qml)
  newXMLNode("rasterrenderer", attrs=c(band="1", type="singlebandpseudocolor"), parent=qml[1])
  newXMLNode("rastershader", parent=qml[[1]][1])
  newXMLNode("colorrampshader", attrs = c(colorRampType = "INTERPOLATED", clip= "0"), parent=qml[[1]][[1]][1])
  for (i in 1:length(bpoints)) {
    newXMLNode("item", attrs = c(alpha="255", value=bpoints[i], label=as.character(format(bpoints[i], digits=2)), color=bcolours[i]), parent=qml[[1]][[1]][[1]][1])
  }
  return(qml)
}

# library(XML)
# node = newXMLNode(c('qgis'))
# newXMLNode("pipe", parent=node)
# newXMLNode("rasterrenderer", attrs=c(band="1", type="singlebandpseudocolor"), parent=node[1])
# newXMLNode("rastershader", parent=node[[1]][1])
# newXMLNode("colorrampshader", attrs = c(colorRampType = "INTERPOLATED", clip= "0"), parent=node[[1]][[1]][1])
# newXMLNode("item", attrs = c(alpha="255", value="0", label="0.01", color="#fdcc8a"), parent=node[[1]][[1]][[1]][1])
# newXMLNode("item", attrs = c(alpha="255", value="0", label="0.01", color="#fdcc8a"), parent=node[[1]][[1]][[1]][1])
# newXMLNode("item", attrs = c(alpha="255", value="0", label="0.01", color="#fdcc8a"), parent=node[[1]][[1]][[1]][1])

### Full example of QML file for raster
# <!DOCTYPE qgis PUBLIC 'http://mrcc.com/qgis.dtd' 'SYSTEM'>
# <qgis version="2.8.2-Wien" minimumScale="-4.65661e-10" maximumScale="1e+08" hasScaleBasedVisibilityFlag="0">
#    <pipe>
#       <rasterrenderer opacity="1" alphaBand="0" classificationMax="16.3837" classificationMinMaxOrigin="MinMaxFullExtentExact" band="1" classificationMin="0" type="singlebandpseudocolor">
#          <rasterTransparency/>
#          <rastershader>
#                <colorrampshader colorRampType="INTERPOLATED" clip="0">
#                   <item alpha="255" value="0" label="0.01" color="#fdcc8a"/>
#                   <item alpha="255" value="6" label="6" color="#fc8d59"/>
#                   <item alpha="255" value="16.3837" label="16.4" color="#d7301f"/>
#                </colorrampshader>
#             </rastershader>
#       </rasterrenderer>
#       <brightnesscontrast brightness="0" contrast="0"/>
#       <huesaturation colorizeGreen="128" colorizeOn="0" colorizeRed="255" colorizeBlue="128" grayscaleMode="0" saturation="0" colorizeStrength="100"/>
#       <rasterresampler maxOversampling="2"/>
#     </pipe>
#     <blendMode>5</blendMode>
# </qgis>