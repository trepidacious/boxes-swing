package org.rebeam.boxes.swing.icons

import javax.swing.{ImageIcon, Icon}
import javax.imageio.ImageIO
import java.awt.image.BufferedImage
import java.awt.{Graphics2D, Color, Image}
import java.net.URL

sealed trait IconSize
object IconSize {
  case object Small
  case object Medium
  case object Large
}

import IconSize._

/**
 * Factory creating {@link Icon} and {@link Image} instances
 * according to a size, category and name. This ties in well
 * with standard icon libraries using the standards at
 * http://standards.freedesktop.org/icon-naming-spec/icon-naming-spec-latest.html
 */
trait IconFactory {
  def image(name: String, resourceClass: Class[_], path: String , extension: String ): Image
  def icon(name: String, resourceClass: Class[_], path: String, extension: String): Icon
  def resource(name: String, resourceClass: Class[_], path: String, extension: String): URL 
}

/**
 * An {@link IconFactory} loading images as png's stored
 * in a standard directory structure and retrieved as resources
 * relative to a given class
 */
//class ResourceIconFactory(val resourceClass:Class[_], val sizeStrings:Map[IconSize, String] = Map(Small -> "small", Medium -> "medium", Large -> "large")) extends IconFactory {
//
//  override def icon(size:IconSize, category:String, name:String) = new ImageIcon(image(size, category, name))
//
//  override def image(size:IconSize, category:String, name:String) = {
//    val sizeString = sizeStrings(size)
//    val location = sizeString + "/" + category + "/" + name + ".png";
//    IconFactory.image(location, resourceClass)
//  }
//}

object IconFactory extends IconFactory {

  val defaultImage = createDefaultImage
  val defaultPath = "/org/rebeam/boxes/swing/icons/"
  val defaultExtension = ".png"

  private def createDefaultImage() = {
    val image = new BufferedImage(16, 16, BufferedImage.TYPE_INT_ARGB)
    val g = image.getGraphics.asInstanceOf[Graphics2D]
    g.setColor(Color.RED)
    g.drawLine(2, 2, 14, 14)
    g.drawLine(14, 2, 2, 14)
    image
  }

  def resource(name: String, resourceClass: Class[_] = classOf[IconFactory], path: String = defaultPath, extension: String = defaultExtension): URL = resourceClass.getResource(path + name + extension)

  def image(name: String, resourceClass: Class[_] = classOf[IconFactory], path: String = defaultPath, extension: String = defaultExtension): Image = {
    val r = resource(name, resourceClass, path, extension)
    if (r != null) {
      try {
        return ImageIO.read(r)
      } catch {
        case _: java.io.IOException => return defaultImage
      }
    } else {
      defaultImage
    }
  }
  
  def icon(name: String, resourceClass: Class[_] = classOf[IconFactory], path: String = defaultPath, extension: String = defaultExtension): Icon = new ImageIcon(image(name, resourceClass, path, extension))
}
