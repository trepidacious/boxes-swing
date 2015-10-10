package org.rebeam.boxes.swing.views

import org.rebeam.boxes.core._
import org.rebeam.boxes.core.util._
import org.rebeam.boxes.swing._
import java.awt.Dimension
import javax.swing.JTextField
import java.awt.event.ActionListener
import java.awt.event.ActionEvent
import java.awt.event.FocusListener
import java.awt.event.FocusEvent
import BoxUtils._
import BoxScriptImports._
import BoxTypes._

//TODO use a renderer to customise display
private class LabelOptionView[G](v: BoxR[G], c: GConverter[G, String]) extends SwingView {

  val component = new LinkingJLabel(this)

  val observer = SwingView.observer(this, v){display(_)}

  atomic{
    import BoxScriptImports._
    observe(observer)
  } 

  //Update display if necessary
  private def display(s: G) {
    val text = c.toOption(s) match {
      case None => ""
      case Some(string) => string
    }
    if (!component.getText.equals(text)) {
      component.setText(text)
    }
  }
}

object LabelView {
  def apply(v: BoxR[String]) = new LabelOptionView(v, new TConverter[String]).asInstanceOf[SwingView]
}

object LabelOptionView {
  def apply(v: BoxR[Option[String]]) = new LabelOptionView(v, new OptionTConverter[String]).asInstanceOf[SwingView]
}

object EmbossedLabelView {
  def apply(v: BoxR[String]) = {
    val view = new LabelOptionView(v, new TConverter[String])
    view.component.setUI(new EmbossedLabelUI())
    view.asInstanceOf[SwingView]
  }
}

object EmbossedLabelOptionView {
  def apply(v: BoxR[Option[String]]) = {
    val view = new LabelOptionView(v, new OptionTConverter[String]) 
    view.component.setUI(new EmbossedLabelUI())
    view.asInstanceOf[SwingView]
  }
}