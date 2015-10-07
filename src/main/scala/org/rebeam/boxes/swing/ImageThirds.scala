package org.rebeam.boxes.swing

import java.awt.Image
import com.explodingpixels.widgets.ImageUtils

case class ImageThirds(pieceWidth: Int, pieceHeight: Int, parts: (Image, Image, Image))

object ImageThirds {
  def horizontalImageThirds(image: Image) = {
    val pieceWidth = image.getWidth(null)/3
    val pieceHeight = image.getHeight(null)
    ImageThirds(
      pieceWidth, pieceHeight,
      (
        ImageUtils.getSubImage(image, 0, 0, pieceWidth, pieceHeight),
        ImageUtils.getSubImage(image, pieceWidth, 0, pieceWidth, pieceHeight),
        ImageUtils.getSubImage(image, pieceWidth * 2, 0, pieceWidth, pieceHeight)
      )
    )
  }

  def verticalImageThirds(image: Image) = {
    val pieceHeight = image.getHeight(null)/3
    val pieceWidth = image.getWidth(null)
    ImageThirds(
      pieceWidth, pieceHeight,
      (
        ImageUtils.getSubImage(image, 0, 0,               pieceWidth, pieceHeight),
        ImageUtils.getSubImage(image, 0, pieceHeight,     pieceWidth, pieceHeight),
        ImageUtils.getSubImage(image, 0, pieceHeight * 2, pieceWidth, pieceHeight)
      )
    )
  }
}
