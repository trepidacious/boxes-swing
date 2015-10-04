package org.rebeam.boxes.swing.views

import org.rebeam.boxes.core._
import org.rebeam.boxes.core.util._
import org.rebeam.boxes.swing.views._
import org.rebeam.boxes.swing._
import java.awt.Dimension
import javax.swing.JTextField
import java.awt.event.ActionListener
import java.awt.event.ActionEvent
import java.awt.event.FocusListener
import java.awt.event.FocusEvent
import javax.swing.SpinnerNumberModel
import javax.swing.JSpinner.DefaultEditor
import java.text.ParseException
import com.explodingpixels.painter.MacWidgetsPainter
import java.awt.{Component, Graphics2D}

import BoxUtils._
import BoxTypes._

object PieView {
  def apply(n:Box[Double], a:Box[Double]) = new PieOptionView(n, new TConverter[Double], a, new TConverter[Double]).asInstanceOf[SwingView]
}

object PieOptionView {
  def apply(n:Box[Option[Double]], a:Box[Option[Double]]) = new PieOptionView(n, new OptionTConverter[Double], a, new OptionTConverter[Double]).asInstanceOf[SwingView]
}

private class PieOptionView[G, H](n: Box[G], c: GConverter[G, Double], a: Box[H], d: GConverter[H, Double]) extends SwingView {

  val pie = PiePainter()

  val component:LinkingEPPanel = new LinkingEPPanel(this);

  {
    component.setBackgroundPainter(new MacWidgetsPainter[Component] {
      override def paint(g: Graphics2D, t: Component, w: Int, h: Int) {
        pie.paint(g, nDisplay, w, h, aDisplay)
      }
    })
    component.setPreferredSize(new Dimension(24, 24))
    component.setMinimumSize(new Dimension(24, 24))
  }

  var nDisplay = 0d
  var aDisplay = 0d

  //Update delegate from Box
  val observer = {
    import BoxObserverScriptImports._

    //TODO use applicative or similar to make this neater
    val script = for {
      newN <- n()
      newA <- a()
    } yield (newN, newA)  

    SwingView.observer(this, script){case (newN, newA) => {
      nDisplay = c.toOption(newN).getOrElse(0d)
      aDisplay = d.toOption(newA).getOrElse(0d)
      component.repaint()      
    }}
  }

  atomic{
    import BoxScriptImports._
    observe(observer)
  } 

}
