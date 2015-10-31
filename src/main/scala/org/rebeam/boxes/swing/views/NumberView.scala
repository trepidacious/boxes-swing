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
import BoxScriptImports._

object NumberView {
  def apply[N](v: BoxM[N])(implicit n: Numeric[N], nc: NumericClass[N]): SwingView = apply(v, nc.defaultSequence)
  def apply[N](v: BoxM[N], s: Sequence[N])(implicit n: Numeric[N], nc: NumericClass[N]) = new NumberOptionView(v, s, new TConverter[N], n, nc).asInstanceOf[SwingView]
}

object NumberOptionView {
  def apply[N](v:BoxM[Option[N]])(implicit n:Numeric[N], nc:NumericClass[N]): SwingView = apply(v, nc.defaultSequence)
  def apply[N](v:BoxM[Option[N]], s:Sequence[N])(implicit n:Numeric[N], nc:NumericClass[N]): SwingView = new NumberOptionView(v, s, new OptionTConverter[N], n, nc).asInstanceOf[SwingView]
}

private class NumberOptionView[G, N](v: BoxM[G], s: Sequence[N], c: GConverter[G, N], n: Numeric[N], nc: NumericClass[N]) extends SwingView {

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
            case e:ParseException => update(atomic{v()})
          }
        }

        override def focusGained(e:FocusEvent) {}

      })
    }
  }

  val observer = SwingView.observer(this, v()){v => update(v)}

  atomic { observe(observer) } 

  def update(newV: G) = {
    // println("update(" + newV + ")")
    c.toOption(newV) match {    
      case None => {
        if (component.isEnabled()) {
          component.setEnabled(false)
        }
        // println("update(" + newV + "), will fire new value " + n.zero)
        model.fireNewValue(n.zero)
      }
      case Some(someNewV) => {
        if (!component.isEnabled()) {
          component.setEnabled(true)
        }
        // println("update(" + newV + "), will fire new value " + someNewV)
        model.fireNewValue(someNewV)
      }
    }
  }

  //FIXME there is an issue with JSpinner where it can end up on a value like 0.21000000000000002,
  //when we decrease this the text component first commits itself as 0.21, then we decrement and hit 0.20.
  //This generates two changes to the viewed Var, which is slightly annoying. 
  private class AutoSpinnerModel extends SpinnerNumberModel {
    import BoxScriptImports._

    private var firing = false
    var currentValue = n.zero

    def fireNewValue(newValue: N) = {
      // println("fireNewValue(" + newValue + "), currentValue " + currentValue)
      if (currentValue != newValue) {
        currentValue = newValue

        //TODO - why DOES fireStateChanged end up calling setValue? can we stop it
        //and avoid the need for firing variable?
        firing = true
        // println("fireNewValue firing new value " + newValue)
        fireStateChanged
        firing = false
        // println("fireNewValue has fired new value " + newValue)
      }
    }

    //These three are nasty - but SpinnerNumberModel expects an Object, and we
    //stupidly have a much nicer instance of N
    override def getNextValue = s.next(currentValue).asInstanceOf[Object]
    override def getPreviousValue = s.previous(currentValue).asInstanceOf[Object]
    override def getValue = currentValue.asInstanceOf[Object]

    override def setValue(spinnerValue: Object) {
      // println("setValue(" + spinnerValue + "), currentValue " + currentValue + ", firing " + firing)
      //Don't respond to our own changes, or incorrect classes
      if (!firing && nc.javaWrapperClass.isAssignableFrom(spinnerValue.getClass)) {
      // if (nc.javaWrapperClass.isAssignableFrom(spinnerValue.getClass)) {
        // currentValue = spinnerValue.asInstanceOf[N]
        // atomic( v() = c.toG(currentValue) )
        val newValue = spinnerValue.asInstanceOf[N]
        val newValueG = c.toG(newValue)
        // println("setValue will set new value " + newValue)
        fireNewValue(newValue)
        atomic{ v() = newValueG }
      }
    }
  }

}