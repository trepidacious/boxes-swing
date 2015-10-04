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
import javax.swing.{DefaultBoundedRangeModel, BoundedRangeModel}

import BoxUtils._
import BoxTypes._
import BoxScriptImports._

object RangeView {
  def apply(v: Box[Int], min: Int, max: Int, progress: Boolean = false) = new RangeOptionView(v, min, max, new TConverter[Int], progress).asInstanceOf[SwingView]
}

object RangeOptionView {
  def apply(v: Box[Option[Int]], min: Int, max: Int, progress: Boolean = false) = new RangeOptionView(v, min, max, new OptionTConverter[Int], progress).asInstanceOf[SwingView]
}

private class RangeOptionView[G](v: Box[G], min: Int, max: Int, c: GConverter[G, Int], progress: Boolean) extends SwingView {

  private val model = new AutoBoundedRangeModel(min, max)

  val component = if (!progress) new LinkingJSlider(this, model) else new LinkingJProgressBar(this, model)

  val observer = {
    import BoxObserverScriptImports._
    SwingView.observer(this, v()){
      c.toOption(_) match {
        case None => {
          component.setEnabled(false)
          model.fireNewValue(model.getMinimum)
        }
        case Some(i) => {
          component.setEnabled(true)
          model.fireNewValue(i)
        }
      }      
    }
  }

  atomic{
    import BoxScriptImports._
    observe(observer)
  } 

  private class AutoBoundedRangeModel(min: Int, max: Int) extends DefaultBoundedRangeModel(min, 0, min, max) {

    private var currentValue = 0

    def fireNewValue(i:Int) = {
      //If necessary, extend range to cover value we have seen
      if (i < getMinimum) setMinimum(i)
      if (i > getMaximum) setMaximum(i)

      currentValue = i
      fireStateChanged
    }

    override def getValue = currentValue

    override def getExtent = 0

    override def setValue(n: Int) = currentValue = n

    override def setValueIsAdjusting(b:Boolean) = {
      super.setValueIsAdjusting(b)
      import BoxScriptImports._
      atomic {
        for {
          vNow <- v()
          _ <- c.toOption(vNow) match {
            case None => nothing
            case Some(_) => v() = c.toG(currentValue)
          }
        } yield ()
      }
    }
  }

}
