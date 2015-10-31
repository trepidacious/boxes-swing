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
import BoxTypes._
import BoxUtils._
import BoxScriptImports._

object StringView {
  def apply(v: BoxM[String], multiline: Boolean = false) = new StringOptionView(v, new TConverter[String], multiline).asInstanceOf[SwingView]
//  def apply(v:Box[String], multiline:Boolean)(implicit shelf: Shelf) = new StringOptionView(v, new TConverter[String], multiline).asInstanceOf[SwingView]
//  def apply(v:Box[Option[String]], multiline:Boolean)(implicit shelf: Shelf, d: DummyImplicit) = new StringOptionView(v, new OptionTConverter[String], multiline).asInstanceOf[SwingView]
//  def apply(v:Box[String])(implicit shelf: Shelf) = new StringOptionView(v, new TConverter[String], false).asInstanceOf[SwingView]
//  def apply(v:Box[Option[String]])(implicit shelf: Shelf, d: DummyImplicit) = new StringOptionView(v, new OptionTConverter[String], false).asInstanceOf[SwingView]
}

object StringOptionView {
  def apply(v: BoxM[Option[String]], multiline: Boolean = false) = new StringOptionView(v, new OptionTConverter[String], multiline).asInstanceOf[SwingView]
}

private class StringOptionView[G](v: BoxM[G], c: GConverter[G, String], multiline: Boolean) extends SwingView {

  val text = if (multiline) new BoxesJTextArea(1, 1) else new LinkingJTextField(this)
  
  //TODO need a nice scrollable text area with the minimal scrollbars from ledger view, inside the text area.
  val component = if (multiline) new LinkingTextEPPanel(this, new LinkingTextJScrollPane(this, text)) else text

  val observer = SwingView.observer(this, v()){display(_)}

  {
    if (multiline) {
      component.setMinimumSize(new Dimension(50, 100))
      component.setPreferredSize(new Dimension(50, 100))
    } else {
      text.asInstanceOf[JTextField].addActionListener(new ActionListener() {
        override def actionPerformed(e:ActionEvent) = commit()
      })
    }

    text.addFocusListener(new FocusListener() {
      override def focusLost(e:FocusEvent) = commit()
      //TODO is this necessary?
      override def focusGained(e:FocusEvent) = display(atomic{v()})
    })

    atomic{
      observe(observer)
    } 
  }

  private def commit() = atomic{v() = c.toG(text.getText)}

  //Update display if necessary
  private def display(s: G) {
    val enableAndText = c.toOption(s) match {
      case None => (false, "")
      case Some(string) => (true, string)
    }
    if (text.isEnabled != enableAndText._1) {
      text.setEnabled(enableAndText._1)
    }
    if (!text.getText.equals(enableAndText._2)) {
      text.setText(enableAndText._2)
    }
  }
}

