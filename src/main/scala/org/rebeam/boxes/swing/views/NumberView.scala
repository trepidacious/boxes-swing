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

import BoxUtils._
import BoxTypes._

object NumberView {
  def apply[N](v: Box[N])(implicit n: Numeric[N], nc: NumericClass[N]): SwingView = apply(v, nc.defaultSequence)
  def apply[N](v: Box[N], s: Sequence[N])(implicit n: Numeric[N], nc: NumericClass[N]) = new NumberOptionView(v, s, new TConverter[N], n, nc).asInstanceOf[SwingView]
}

object NumberOptionView {
  def apply[N](v:Box[Option[N]])(implicit n:Numeric[N], nc:NumericClass[N]): SwingView = apply(v, nc.defaultSequence)
  def apply[N](v:Box[Option[N]], s:Sequence[N])(implicit n:Numeric[N], nc:NumericClass[N]): SwingView = new NumberOptionView(v, s, new OptionTConverter[N], n, nc).asInstanceOf[SwingView]
}

private class NumberOptionView[G, N](v: Box[G], s: Sequence[N], c: GConverter[G, N], n: Numeric[N], nc: NumericClass[N]) extends SwingView {

  private val model = new AutoSpinnerModel()
  val component = new LinkingJSpinner(this, model)

  //If the editor is a default editor, we can work around
  //failure to commit when selecting a menu (and possibly other
  //problems) by monitoring the editor, and committing its edits when it loses
  //focus. This should happen automatically, and sometimes does by some
  //means I have not yet located, but fails when immediately selecting a menu
  //after editing, etc.
  component.getEditor() match {
    case dEditor:DefaultEditor => {
      dEditor.getTextField().addFocusListener(new FocusListener() {

        override def focusLost(e:FocusEvent) {
          try {
            component.commitEdit()
          } catch {
            case e:ParseException => update(Shelf.currentRevision)
          }
        }

        override def focusGained(e:FocusEvent) {}

      })
    }
  }

  val observer = new Observer {
    def observe(r:Revision): Unit = update(r)
  }

  atomic(observe(observer))

  def update(r: Revision) = {
    //Store the values for later use on Swing Thread
    val newV = v(r)
    //This will be called from Swing Thread
    SwingView.replaceUpdate(this, 
    {
      c.toOption(newV) match {
        case None => {
          component.setEnabled(false)
          model.fireNewValue(n.zero)
        }
        case Some(someNewV) => {
          component.setEnabled(true)
          model.fireNewValue(someNewV)
        }
      }
    })
  }

  //FIXME there is an issue with JSpinner where it can end up on a value like 0.21000000000000002,
  //when we decrease this the text component first commits itself as 0.21, then we decrement and hit 0.20.
  //This generates two changes to the viewed Var, which is slightly annoying. 
  private class AutoSpinnerModel extends SpinnerNumberModel {
    private var firing = false
    var currentValue = n.zero

    def fireNewValue(newValue:N) = {
      currentValue = newValue

      //TODO - why DOES fireStateChanged end up calling setValue? can we stop it
      //and avoid the need for firing variable?
      firing = true
      fireStateChanged
      firing = false
    }

    //These three are nasty - but SpinnerNumberModel expects an Object, and we
    //stupidly have a much nicer instance of N
    override def getNextValue = s.next(currentValue).asInstanceOf[Object]
    override def getPreviousValue = s.previous(currentValue).asInstanceOf[Object]
    override def getValue = currentValue.asInstanceOf[Object]

    override def setValue(spinnerValue:Object) {
      //Don't respond to our own changes, or incorrect classes
      if (!firing && nc.javaWrapperClass.isAssignableFrom(spinnerValue.getClass)) {
        currentValue = spinnerValue.asInstanceOf[N]
        atomic( v() = c.toG(currentValue) )
      }
    }
  }

}