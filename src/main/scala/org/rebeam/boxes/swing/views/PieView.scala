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
import BoxScriptImports._

object PieView {
  def apply(n: BoxR[Double], a: BoxR[Double]) = new PieOptionView(n, new TConverter[Double], a, new TConverter[Double]).asInstanceOf[SwingView]
}

object PieOptionView {
  def apply(n: BoxR[Option[Double]], a: BoxR[Option[Double]]) = new PieOptionView(n, new OptionTConverter[Double], a, new OptionTConverter[Double]).asInstanceOf[SwingView]
}

private class PieOptionView[G, H](n: BoxR[G], c: GConverter[G, Double], a: BoxR[H], d: GConverter[H, Double]) extends SwingView {

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
    //TODO use applicative or similar to make this neater
    val script = for {
      newN <- n
      newA <- a
    } yield (newN, newA)  

    SwingView.observer(this, script){case (newN, newA) => {
      nDisplay = c.toOption(newN).getOrElse(0d)
      aDisplay = d.toOption(newA).getOrElse(0d)
      component.repaint()      
    }}
  }

  atomic{ observe(observer) } 

}
