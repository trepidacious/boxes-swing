package org.rebeam.boxes.swing.demo

import javax.swing.{JFrame, JPanel, JButton, JScrollPane, AbstractAction, SwingUtilities}
import java.awt.{BorderLayout, Dimension}
import java.awt.event.ActionEvent

import java.text.DecimalFormat

import org.rebeam.boxes.swing.views._
import org.rebeam.boxes.swing.layout._
import org.rebeam.boxes.swing._
import org.rebeam.boxes.core._
import org.rebeam.boxes.core.data._

import BoxUtils._
import BoxTypes._
import BoxScriptImports._

import scalaz._
import Scalaz._

import scala.collection.immutable._

object LedgerDemoApp extends App {
  
  case class Person(
    name: Box[String], 
    age: Box[Int],
    friend: Box[Option[Person]]
  ) 

  object Person {
    def default: BoxScript[Person] = default("Unnamed", 20, None)

    def default(
      name: String = "", 
      age: Int = 20, 
      friend: Option[Person] = None
    ): BoxScript[Person] = for {
      n <- create(name)
      a <- create(age)
      f <- create(friend)
    } yield Person(n, a, f)
  }
  
  def ledger() {
    
    val p = atomic { Person.default("p", 20) }
    val q = atomic { Person.default("q", 21) }

    val list = atomic { create(IndexedSeq(p, q)) }

    val view = atomic { 
      create[RecordView[Person]](
        LensRecordView[Person](
          MBoxLens("Name", _.name),
          MBoxLens("Age", _.age)
        )
      )
    }
    
    val ledger = atomic {
      ListLedgerBox[Person](list, view)
    }

    val li = atomic(ListIndex(list))
    
    val i = li.index
    
    val selectedName = atomic { 
      cal(
        for {
          s <- li.selected()
          n <- s.traverseU(_.name())
        } yield n.getOrElse("No selection")
      )
    }

    val selectedNameView = LabelView(selectedName)
    
    val ledgerView = LedgerView.singleSelectionScroll(ledger, i, true)
    
    val indexView = NumberOptionView(i)

    val next = atomic { create(0) }
    
    val add = new JButton(new AbstractAction("Add") {
      override def actionPerformed(e:ActionEvent) = atomic {
        for {
          n <- next()
          p <- Person.default("New item " + n, 20 + n)
          _ <- next() = n + 1
          l <- list()
          _ <- list() = IndexedSeq(p) ++ l
        } yield ()
      }
    })

    val delete = new JButton(new AbstractAction("Delete") {
      override def actionPerformed(e:ActionEvent) = atomic {
        for {
          l <- list()
          _ <- if (!l.isEmpty) list() = l.tail else nothing          
        } yield ()
      }
    })

    val frame = new JFrame()
    val panel = new JPanel()
    panel.add(add)
    panel.add(delete)
    panel.add(indexView.component)
    panel.add(selectedNameView.component)
    frame.add(ledgerView.component, BorderLayout.CENTER)
    frame.add(panel, BorderLayout.SOUTH)
    frame.pack
    frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
    frame.setVisible(true)

  }
  
  SwingView.later{
    SwingView.nimbus
    ledger
  }
  
}