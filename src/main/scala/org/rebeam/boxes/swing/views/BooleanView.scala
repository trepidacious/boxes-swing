package org.rebeam.boxes.swing.views

import org.rebeam.boxes.core.util._
import org.rebeam.boxes.core._
import org.rebeam.boxes.swing._
import java.awt.Dimension
import javax.swing.JTextField
import java.awt.event.ActionListener
import java.awt.event.ActionEvent
import java.awt.event.FocusListener
import java.awt.event.FocusEvent
import javax.swing.Icon
import javax.swing.JToggleButton.ToggleButtonModel

import BoxUtils._
import BoxTypes._

import BoxScriptImports._

import scalaz._
import Scalaz._

sealed trait BooleanControlType
case object Checkbox extends BooleanControlType
case object ToggleButton extends BooleanControlType
case object ToolbarButton extends BooleanControlType
case object SlideCheck extends BooleanControlType
case object Radio extends BooleanControlType
case object Tab extends BooleanControlType

object BooleanView {
  def extended(v: Box[Boolean], n: Option[Box[String]], controlType: BooleanControlType = SlideCheck, icon: Option[Box[Icon]], toggle: Boolean = true) = new BooleanOptionView(v, n, new TConverter[Boolean], controlType, icon, toggle).asInstanceOf[SwingView]
  def apply(v: Box[Boolean], controlType: BooleanControlType = SlideCheck, toggle: Boolean = true) = new BooleanOptionView(v, None, new TConverter[Boolean], controlType, None, toggle).asInstanceOf[SwingView]
  def toolbar(v: Box[Boolean], icon: Option[Box[Icon]], toggle: Boolean = true) = new BooleanOptionView(v, None, new TConverter[Boolean], ToolbarButton, icon, toggle).asInstanceOf[SwingView]
}

object BooleanOptionView {
  def extended(v: Box[Option[Boolean]], n: Box[String], controlType: BooleanControlType = SlideCheck, icon: Option[Box[Icon]], toggle: Boolean = true) = new BooleanOptionView(v, Some(n), new OptionTConverter[Boolean], controlType, icon, toggle).asInstanceOf[SwingView]
  def apply(v: Box[Option[Boolean]], controlType: BooleanControlType = SlideCheck, toggle: Boolean = true) = new BooleanOptionView(v, None, new OptionTConverter[Boolean], controlType, None, toggle).asInstanceOf[SwingView]
}

private class BooleanOptionView[G](v: Box[G], n: Option[Box[String]], c: GConverter[G, Boolean], controlType: BooleanControlType, icon: Option[Box[Icon]], toggle: Boolean = true) extends SwingView {

  val component = controlType match {
    case Checkbox => new LinkingJCheckBox(this)
    case Radio => new LinkingJRadioButton(this)
    case ToggleButton => new LinkingJToggleButton(this)
    case ToolbarButton => new LinkingToolbarToggleButton(this)
    case SlideCheck => new LinkingSlideCheckButton(this)
    case Tab => new LinkingTabButton(this)
  }

  private val model = new AutoButtonModel()

  //Update delegate from Box
  val observer = {
    //TODO use applicative or similar to make this neater
    val script = for {
      newV <- v()
      newN <- n.traverseU{_()}        //With an Option[Box[String]] and a Box[String]=>Script[String] we can get an Option[Script[String]], traverseU gives us a Script[Option[String]] instead, which is what we want
      newIcon <- icon.traverseU{_()}
    } yield (newV, newN, newIcon)  

    SwingView.observer(this, script){v => display(v._1, v._2, v._3)}
  }

  {
    component.setModel(model)
    component.addActionListener(new ActionListener(){
      //On action, toggle value if it is not None
      override def actionPerformed(e:ActionEvent) = atomic {
        for {
          value <- v()
          _ <- c.toOption(value) match {
            case None => nothing
            case Some(b) => v() = if (toggle) c.toG(!b) else c.toG(true)
          }
        } yield ()        
      }
    })
    
    atomic { observe(observer) } 
  }

  //Update display if necessary
  private def display(newV: G, newN: Option[String], newIcon: Option[Icon]) {
    c.toOption(newV) match {
      case None => {
        model.enabled = false
        model.selected = false
      }
      case Some(b) => {
        model.enabled = true
        model.selected = b
      }
    }
    model.fire
    val newNValue = newN.getOrElse("")
    if (newNValue != component.getText) {
      component.setText(newNValue)
    }
    val iconOrNull = newIcon.getOrElse(null)
    if (iconOrNull != component.getIcon) {
      component.setIcon(iconOrNull)
    }
  }

  private class AutoButtonModel extends ToggleButtonModel {
    var enabled = true
    var selected = true
    def fire() = fireStateChanged()
    override def isSelected = selected
    override def isEnabled = enabled
  }

}

