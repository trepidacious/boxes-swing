package org.rebeam.boxes.swing.layout

import java.awt.{CardLayout, BorderLayout, Dimension, Container, Component, LayoutManager}
import javax.swing.{JPanel, Icon, JComponent}
import org.rebeam.boxes.core._
import org.rebeam.boxes.core.reaction._
import org.rebeam.boxes.swing._
import org.rebeam.boxes.swing.views._
import BoxUtils._
import BoxTypes._
import BoxScriptImports._

import scalaz._
import Scalaz._

case class PartialViewInfo[A, B](b: BoxR[A], d: B, v: BoxR[B] => SwingView, pf: PartialFunction[A, B])

case class PartialViewBuilder[A](b: BoxR[A], info: List[(A => Boolean, SwingView)]) {

  def add[B](d: B, v: BoxR[B] => SwingView)(pf: PartialFunction[A, B]) = {
    val boxB = b.partialOrDefault(pf)(d)
    val view = v(boxB)
    val newInfo = ((a: A) => pf.isDefinedAt(a), view)
    PartialViewBuilder(b, newInfo :: info)
  }

  def build() = PartialViewBuilder.build(b, info)
}

case class PartialOptionViewBuilder[A](b: BoxR[Option[A]], info: List[(Option[A] => Boolean, SwingView)]) {

  def add[B](d: B, v: BoxR[B] => SwingView)(pf: PartialFunction[A, B]) = {
    val boxB = b.partialOrDefault(pf)(d)
    val view = v(boxB)
    val newInfo = ((oa: Option[A]) => oa.map(a => pf.isDefinedAt(a)).getOrElse(false), view)
    PartialViewBuilder(b, newInfo :: info)
  }

  def build() = PartialViewBuilder.build(b, info)
}

object PartialViewBuilder {
  def of[A](b: BoxR[A]) = PartialViewBuilder(b, Nil) 
  def ofOption[A](b: BoxR[Option[A]]) = PartialOptionViewBuilder(b, Nil) 

  def build[A](b: BoxR[A], info: List[(A => Boolean, SwingView)]) = {
    val cardLayout = new CardLayout()
    val contentPanel = new JPanel(cardLayout)

    val infoInOrder = info.reverse

    //Blank panel for when nothing is selected
    contentPanel.add(new JPanel(), "-1")

    //Add our views by index
    infoInOrder.zipWithIndex.foreach{case(info, index) => contentPanel.add(info._2.component, index.toString)}

    val definedInOrder = infoInOrder.map(_._1)

    //Show the selected content panel card
    val observer = SwingView.observer(this, b){ b => 
      val index = definedInOrder.indexWhere(_(b))
      cardLayout.show(contentPanel, index.toString)
    }
    atomic(observe(observer))

    //Observer would be lost if not retained by panel
    val panel = new LinkingJPanel(observer, new BorderLayout())
    panel.add(contentPanel, BorderLayout.CENTER)
    panel
  }
}

// case class TabBuilder(toggles: List[Box[Boolean]] = List(), tabComponents: List[JComponent] = List(), contentComponents: List[JComponent] = List()) {

//   def add(contents: JComponent, name: BoxR[String], icon: BoxR[Option[Icon]] = just(None), v: Box[Boolean] = atomic(create(toggles.isEmpty))): TabBuilder = {
//     val view = BooleanView.extended(v, name, Tab, icon, false)    
//     TabBuilder(toggles:::List(v), tabComponents:::List(view.component), contentComponents:::List(contents))
//   }

//   def addView(contents: SwingView, name: BoxR[String], icon: BoxR[Option[Icon]] = just(None), v: Box[Boolean] = atomic(create(toggles.isEmpty))): TabBuilder = add(contents.component, name, icon, v)

//   def panel(width:Int = 64, height:Int = 64) = {
//     atomic(RadioReaction(toggles))
//     val tabPanel = new JPanel(VerticalTabLayout(width, height))
//     tabComponents.foreach(c => tabPanel.add(c))

//     val cardLayout = new CardLayout()
//     val contentPanel = new JPanel(cardLayout)

//     //Blank panel for when nothing is selected
//     contentPanel.add(new JPanel(), "-1")
//     contentComponents.zipWithIndex.foreach{case(c, i) => contentPanel.add(c, i.toString)}

//     //Show the selected content panel card
//     val observer = SwingView.observer(this, toggles.traverse(_())){ t => 
//       val index = t.indexWhere(b => b)
//       cardLayout.show(contentPanel, index.toString)
//     }

//     atomic(observe(observer))

//     val sidePanel = new JPanel(new BorderLayout())
//     sidePanel.add(tabPanel, BorderLayout.NORTH)
//     sidePanel.add(TabSpacer(), BorderLayout.CENTER)

//     //Observer would be lost if not retained by panel
//     val panel = new LinkingJPanel(observer, new BorderLayout())
//     panel.add(sidePanel, BorderLayout.WEST)
//     panel.add(contentPanel, BorderLayout.CENTER)
//     panel
//   }

// }

