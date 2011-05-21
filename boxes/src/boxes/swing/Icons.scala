package boxes.swing

import javax.swing.{ImageIcon, Icon}
import javax.imageio.ImageIO
import java.awt.image.BufferedImage
import java.awt.{Graphics2D, Color, Image}
import java.net.URL

object IconSize extends Enumeration {
   type IconSize = Value
   val Small, Medium, Large = Value
 }

import IconSize._

/**
 * Factory creating {@link Icon} and {@link Image} instances
 * according to a size, category and name. This ties in well
 * with standard icon libraries using the standards at
 * http://standards.freedesktop.org/icon-naming-spec/icon-naming-spec-latest.html
 */
trait IconFactory {

	def icon(size:IconSize, category:String, name:String):Icon
  def image(size:IconSize, category:String, name:String):Image
}

/**
 * An {@link IconFactory} loading images as png's stored
 * in a standard directory structure and retrieved as resources
 * relative to a given class
 */
class ResourceIconFactory(val resourceClass:Class[_], val sizeStrings:Map[IconSize, String] = Map(Small -> "small", Medium -> "medium", Large -> "large")) extends IconFactory {

  override def icon(size:IconSize, category:String, name:String) = new ImageIcon(image(size, category, name))

  override def image(size:IconSize, category:String, name:String) = {
    val sizeString = sizeStrings(size)
		val location = sizeString + "/" + category + "/" + name + ".png";
    IconFactory.image(resourceClass, location)
  }
}

object IconFactory {

  val defaultImage = createDefaultImage

  private def createDefaultImage() = {
    val image = new BufferedImage(16, 16, BufferedImage.TYPE_INT_ARGB)
    val g = image.getGraphics.asInstanceOf[Graphics2D]
    g.setColor(Color.RED)
    g.drawLine(2, 2, 14, 14)
    g.drawLine(14, 2, 2, 14)
    image
  }

  def image(resourceClass:Class[_], location:String):Image = {
    val resource:URL = resourceClass.getResource(location)
		if (resource != null) {
			try {
				return ImageIO.read(resource)
			} catch {
				case _ => return defaultImage
      }
		} else {
      defaultImage
    }
  }
  def icon(resourceClass:Class[_], s:String) = new ImageIcon(image(resourceClass, s))
}
