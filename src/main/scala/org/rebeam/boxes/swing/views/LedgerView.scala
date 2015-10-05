package org.rebeam.boxes.swing.views

import java.util.concurrent.atomic.AtomicBoolean
import scala.collection.immutable
import javax.swing.event.ChangeEvent
import javax.swing.event.TableColumnModelEvent
import javax.swing.event.TableModelEvent
import javax.swing.table.AbstractTableModel
import javax.swing.table.TableCellEditor
import javax.swing.table.TableCellRenderer
import javax.swing.table.TableModel
import javax.swing.{JTable, JPanel, JComponent, Action, KeyStroke, AbstractAction}
import java.awt.BorderLayout
import java.awt.event.{KeyEvent, ActionEvent, MouseListener, MouseEvent}
import java.awt.Dimension
import scala.collection.mutable.ListBuffer

import org.rebeam.boxes.core._
import org.rebeam.boxes.core.data._
import org.rebeam.boxes.swing.selection._
import org.rebeam.boxes.swing._

import BoxUtils._
import BoxTypes._

import BoxScriptImports._

import scalaz._
import Scalaz._

object LedgerView {

  def apply(v: Box[Ledger], sorting: Boolean = false) = {
    val lv = new LedgerView(v)
    if (sorting) lv.component.setAutoCreateRowSorter(true)
    lv
  }

  def singleSelection(v: Box[Ledger], i: Box[Option[Int]], sorting: Boolean = false) = {
    val lv = new LedgerView(v)
    //Only allow the selection to be set when the table is NOT responding
    //to a model change.
    //This is somewhat messy, but is necessary to wrest control of updating the selection
    //away from the JTable - we already update the selection ourself in a more intelligent
    //way, so we only want the selection changes that are NOT in response to a table model
    //change, but in response to a user selection action
    lv.component.setSelectionModel(new ListSelectionIndexModel(i, !lv.component.isRespondingToChange, lv.component))
    if (sorting) lv.component.setAutoCreateRowSorter(true)
    lv
  }
  
  def multiSelection(v: Box[Ledger], i: Box[Set[Int]], sorting: Boolean = false) = {
    val lv = new LedgerView(v)
    lv.component.setSelectionModel(new ListSelectionIndicesModel(i, !lv.component.isRespondingToChange, lv.component))
    if (sorting) lv.component.setAutoCreateRowSorter(true)
    lv
  }

  def singleSelectionScroll(v: Box[Ledger], i: Box[Option[Int]], sorting: Boolean = false) = {
    import BoxScriptImports._
    val lv = new LedgerView(v)
    lv.component.setSelectionModel(new ListSelectionIndexModel(i, !lv.component.isRespondingToChange, lv.component))

    val iAsSet = atomic(cal(i().map(_.toSet)))

    val lsv = new LedgerScrollView(lv, v, iAsSet)
      
    if (sorting) lv.component.setAutoCreateRowSorter(true)
    lsv
  }

  def multiSelectionScroll(v: Box[Ledger], i: Box[Set[Int]], sorting: Boolean = false) = {
    val lv = new LedgerView(v)
    lv.component.setSelectionModel(new ListSelectionIndicesModel(i, !lv.component.isRespondingToChange, lv.component))
    val lsv = new LedgerScrollView(lv, v, i)
    if (sorting) lv.component.setAutoCreateRowSorter(true)
    lsv
  }

//  def list[T](list:ListVar[T], view:Box[RecordView[T], _], i:VarBox[Option[Int], _], sorting:Boolean, source: => Option[T], target:T => Unit, component:JComponent, additionalViews:SwingView*) = {
//    
//    val ledger = ListLedgerVar(list, view)
//    val ledgerView = singleSelectionScroll(ledger, i, sorting)
//
//    val add = new ListAddOp(list, i, source)
//    val delete = new ListDeleteOp(list, i, target)
//
//    val up = new ListMoveOp(list, i, true)
//    val down = new ListMoveOp(list, i, false)
//
//    val buttons = SwingButtonBar().add(add).add(delete).add(up).add(down);
//    val panel = additionalViews.foldLeft(buttons){case (b, a) => b.add(a)}.buildWithListStyleComponent(component)
//
//    val mainPanel = new JPanel(new BorderLayout())
//    mainPanel.add(ledgerView.component, BorderLayout.CENTER)
//    mainPanel.add(panel, BorderLayout.SOUTH)
//
//    mainPanel
//  }
//  
//  def listAddPopup[T](list:ListVar[T], 
//      view:Box[RecordView[T], _], 
//      i:VarBox[Option[Int], _], 
//      sorting:Boolean, 
//      source: => List[T], 
//      addView:Box[RecordView[T],_], 
//      target:T => Unit, component:JComponent, 
//      additionalViews:SwingView*) = {
//
//    //Make a ledger view to select from when adding instances
//    val addListVar = ListVar[T](source)
//    val addLedger = ListLedgerVar(addListVar, addView)
//    val addSelection = ListIndices(addListVar)
//    val addLedgerView = multiSelectionScroll(addLedger, addSelection)
//
//    val addPanel = new JPanel(new BorderLayout)
//    addPanel.add(addLedgerView.component)
//    val addPopup = BoxesPopupView(icon = Val(Some(IconFactory.icon("Plus"))), popupContents = addPanel)
//
//    def hideAndResetPopup() {
//      addPopup.hide()
//      addListVar() = source
//      addSelection() = Set[Int]()
//    }
//    
//    val addOp = new Op {
//      val canApply = Val(true)
//    
//      def apply() = {
//        Box.transact{
//          val insertion = i() match {
//            case Some(someI) => someI + 1
//            case None => list().size
//          }
//
//          val itemsToAdd = addSelection().toList.sortWith((x, y) => x > y).map(i => addListVar(i))
//          if (!itemsToAdd.isEmpty) {
//            itemsToAdd.foreach(list.insert(insertion, _))
//            i() = Some(insertion + itemsToAdd.size - 1)
//          }
//        }
//        hideAndResetPopup()
//      }
//    }
//            
//    val addButtons = SwingButtonBar()
//        .add(SwingBarButton(name="Add ", icon = Some(IconFactory.icon("Plus")), op = addOp))
//        .build(SwingBarButton(name="Cancel", icon = None, op=Op{hideAndResetPopup()}))
//    
//    addPanel.add(addButtons, BorderLayout.SOUTH)
//    addPanel.setPreferredSize(new Dimension(300, 300))
//
//    val addAction = new AbstractAction() {
//      override def actionPerformed(e:ActionEvent) {
//        addOp.apply()
//      }
//    }
//  
//    addLedgerView.ledgerView.replaceEnterAction(addAction)
//    addLedgerView.ledgerView.replaceSpaceAction(addAction)
//    addLedgerView.ledgerView.component.addMouseListener(new MouseListener(){
//      def mouseClicked(e: MouseEvent) {
//        if (e.getClickCount() > 1) addOp.apply()
//      }
//      def mousePressed(e: MouseEvent) {
//      }
//      def mouseReleased(e: MouseEvent) {
//      }
//      def mouseEntered(e: MouseEvent) {
//      }
//      def mouseExited(e: MouseEvent) {
//      }
//    })
//    
//    val ledger = ListLedgerVar(list, view)
//    val ledgerView = singleSelectionScroll(ledger, i, sorting)
//    
//    val delete = new ListDeleteOp(list, i, target)
//
//    val up = new ListMoveOp(list, i, true)
//    val down = new ListMoveOp(list, i, false)
//
//    val buttons = SwingButtonBar().add(addPopup).add(delete).add(up).add(down);
//    val panel = additionalViews.foldLeft(buttons){case (b, a) => b.add(a)}.buildWithListStyleComponent(component)
//
//    val mainPanel = new JPanel(new BorderLayout())
//    mainPanel.add(ledgerView.component, BorderLayout.CENTER)
//    mainPanel.add(panel, BorderLayout.SOUTH)
//
//    mainPanel
//  }
}

class LedgerScrollView(val ledgerView: LedgerView, val ledger: Box[Ledger], val indices: Box[Set[Int]]) extends SwingView {
  val component = new LinkingJScrollPane(this, ledgerView.component)
  val dotModel = new DotModel()

  BoxesScrollBarUI.applyTo(component, new DotModel(), dotModel, false, true)
  val table = ledgerView.component

  //FIXME applicative?
  val observer = {
    val script = for {
      l <- ledger()
      scale <- l.recordCount
      is <- indices()
    } yield (scale, is)
    SwingView.observer(this, script){ case (scale, is) =>
      val viewIndices = is.map(i => indexToView(i)).filter(i => i >= 0).toList.sorted
      val viewRuns = encodeDirect(viewIndices)
      val scaleDouble = scale.toDouble
      val viewRunsScaled = viewRuns.map(run => (run._1/scaleDouble, (run._1 + run._2)/scaleDouble))

      dotModel.positions = viewRunsScaled.toSet
    }
  }

  atomic { observe(observer) } 

  //Convert a sorted list of ints to a list of starts and lengths of runs of ints
  def encodeDirect(list:List[Int]) : List[(Int,Int)] = {

    def encode(result:List[(Int,Int)], n:Int, rl:List[Int]) : List[(Int,Int)] = {
      rl match {
        //If the first two elements are a run, continue any current run
        case head::next::tail if head == next-1 => encode(result,n+1,next::tail)
        //Ending a run, with more to encode
        case head::next::tail => encode(result:::List((head-n, n+1)),0,next::tail)
        //Ending a run and no more to encode
        case head::Nil => result:::List((head-n, n+1))
        //Single stage of encoding an empty list
        case Nil => result
      }
    }

    encode(Nil,0,list)
  }

  def indexToView(i:Int):Int = {
    try {
      table.convertRowIndexToView(i)
    } catch {
      //If index is out of bounds, treat as no selection
      case _: IndexOutOfBoundsException => -1
    }
  }
}

class LedgerView(v: Box[Ledger]) extends SwingView {

  var lastFieldNames: Option[List[String]] = None
  var lastFieldClasses: Option[List[Class[_]]] = None
  var lastRecordCount: Option[Int] = None
  
  class LedgerTableModel extends AbstractTableModel {
    override def getColumnClass(columnIndex:Int) = atomic { 
      v().flatMap(_.fieldClass(columnIndex)) 
    }
    override def getColumnName(columnIndex:Int) = atomic { 
      v().flatMap(_.fieldName(columnIndex)) 
    }
    override def getColumnCount() = atomic{ 
      v().flatMap(_.fieldCount) 
    }
    override def getRowCount() = atomic{ 
      v().flatMap(_.recordCount) 
    }
    override def getValueAt(rowIndex:Int, columnIndex:Int) =  atomic { 
      v().flatMap(_.apply(rowIndex, columnIndex)).map(_.asInstanceOf[AnyRef]) 
    } 
    override def isCellEditable(rowIndex:Int, columnIndex:Int) = atomic { 
      v().flatMap(_.editable(rowIndex, columnIndex)) 
    }
    override def setValueAt(aValue:Object, rowIndex:Int, columnIndex:Int) = atomic { 
      for {
        l <- v()
        l2 <- l.updated(rowIndex, columnIndex, aValue)
        _ <- v() = l2
      } yield ()
    }
  }

  val model = new LedgerTableModel()

  val component = new LinkingJTable(this, model)
  
  val observer = {
    val script = for {
      l <- v()
      rc <- l.recordCount
      fc <- l.fieldCount
      columnIndices = Range(0, fc).toList 
      fieldNames <- columnIndices.traverseU(l.fieldName(_))
      fieldClasses <- columnIndices.toList.traverseU(l.fieldClass(_))

      //Read all ledger contents - not neat, but means we receive updates. 
      //Might be best just to cache it all but probably doesn't make much difference
      cellAddresses = (for (r <- Range(0, rc); f <- Range(0, fc)) yield (r, f)).toList
      cells <- cellAddresses.traverseU(a => l(a._1, a._2))
      editabilities <- cellAddresses.traverseU(a => l.editable(a._1, a._2))

    } yield (l, rc, fc, fieldNames, fieldClasses)

    SwingView.observer(this, script){ case (l, rc, fc, fieldNames, fieldClasses) => {

      //Check for column changes
      val columnsChanged = (lastFieldNames, lastFieldClasses) match {
        case (Some(lfn), Some(lfc)) => (lfn != fieldNames || lfc != fieldClasses)
        case _ => true
      }      
      lastFieldNames = Some(fieldNames.toList)
      lastFieldClasses = Some(fieldClasses.toList)

      //Check for row count change
      val rowCountChanged = lastRecordCount match {
        case Some(lrc) => lrc != rc
        case _ => true
      }
      lastRecordCount = Some(rc)
 
      //Fire events suitable for observed changes
      if (columnsChanged) {
        model.fireTableStructureChanged()
      } else if (rowCountChanged) {
        model.fireTableDataChanged()
      } else {
        model.fireTableRowsUpdated(0, rc - 1)
      }
    }}
  }

  atomic { observe(observer) } 

  def defaultEditor(columnClass:Class[_]) = component.getDefaultEditor(columnClass)
  def defaultRenderer(columnClass:Class[_]) = component.getDefaultRenderer(columnClass)

  def defaultEditor(columnClass:Class[_], editor:TableCellEditor) {
    component.setDefaultEditor(columnClass, editor);
  }

  def defaultRenderer(columnClass:Class[_], renderer:TableCellRenderer) {
    component.setDefaultRenderer(columnClass, renderer);
  }

  def rowHeight = component.getRowHeight
  def rowHeight_=(rowHeight:Int) = component.setRowHeight(rowHeight)

  def removeHeader() = component.setTableHeader(null)

  def replaceEnterAction(a: Action) {
    replaceKeyEventAction(a, KeyEvent.VK_ENTER)
  }
  def replaceSpaceAction(a: Action) {
    replaceKeyEventAction(a, KeyEvent.VK_SPACE)
  }

  def replaceKeyEventAction(a: Action, keyCode: Int) {
    component.getInputMap(JComponent.WHEN_ANCESTOR_OF_FOCUSED_COMPONENT)
      .put(KeyStroke.getKeyStroke(keyCode, 0), "boxesNewTableEnterAction");
    component.getInputMap(JComponent.WHEN_FOCUSED)
      .put(KeyStroke.getKeyStroke(keyCode, 0), "boxesNewTableEnterAction");
    component.getActionMap().put("boxesNewTableEnterAction", a);
  }
}

class LinkingJTable(val sv:SwingView, m:TableModel) extends JTable(m) {

  getTableHeader().setDefaultRenderer(new BoxesTableCellHeaderRenderer())

  val defaultRenderer = new BoxesTableCellRenderer()

  //Apologies for null, super constructor calls lots of
  //methods, leading to use of responding before it can be
  //initialised. This is why I hate subclassing, but necessary
  //to make a JTable. We initialise responding wherever it first
  //happens to get used.
  private var responding:AtomicBoolean = null

  setDefaultRenderer(classOf[Boolean],  BooleanCellRenderer.opaque)
  setDefaultRenderer(classOf[Char],     defaultRenderer)
  setDefaultRenderer(classOf[String],     defaultRenderer)

  //We want to use implicits, so we can't use a list of classes, unfortunately
  setDefaultEditor(classOf[Byte],       NumberCellEditor(classOf[Byte]))
  setDefaultEditor(classOf[Double],     NumberCellEditor(classOf[Double]))
  setDefaultEditor(classOf[Long],       NumberCellEditor(classOf[Long]))
  setDefaultEditor(classOf[Float],      NumberCellEditor(classOf[Float]))
  setDefaultEditor(classOf[Int],        NumberCellEditor(classOf[Int]))
  setDefaultEditor(classOf[Short],      NumberCellEditor(classOf[Short]))
  setDefaultEditor(classOf[BigInt],     NumberCellEditor(classOf[BigInt]))
  setDefaultEditor(classOf[BigDecimal], NumberCellEditor(classOf[BigDecimal]))

  setDefaultRenderer(classOf[Byte],       NumberCellRenderer(classOf[Byte]))
  setDefaultRenderer(classOf[Double],     NumberCellRenderer(classOf[Double]))
  setDefaultRenderer(classOf[Long],       NumberCellRenderer(classOf[Long]))
  setDefaultRenderer(classOf[Float],      NumberCellRenderer(classOf[Float]))
  setDefaultRenderer(classOf[Int],        NumberCellRenderer(classOf[Int]))
  setDefaultRenderer(classOf[Short],      NumberCellRenderer(classOf[Short]))
  setDefaultRenderer(classOf[BigInt],     NumberCellRenderer(classOf[BigInt]))
  setDefaultRenderer(classOf[BigDecimal], NumberCellRenderer(classOf[BigDecimal]))


  setDefaultEditor(classOf[String],     SelectingTextCellEditor())
  setDefaultEditor(classOf[Boolean],    BooleanCellEditor())

  //TODO add default editor/renderer for Color

  setRowHeight(24)

  //Workarounds for when not using Nimbus
//  setShowGrid(false)
//  setIntercellSpacing(new Dimension(0, 0))
//  setShowHorizontalLines(false)
//  setShowVerticalLines(false)

  //See Java bug 4709394
  putClientProperty("terminateEditOnFocusLost", true);

  //Workaround for bug 4330950, stops editing before starting to move column
  override def columnMoved(e:TableColumnModelEvent) {
      if (isEditing()) cellEditor.stopCellEditing()
      super.columnMoved(e);
  }

  //Workaround for bug 4330950, stops editing before starting to change column
  override def columnMarginChanged(e:ChangeEvent) {
      if (isEditing()) cellEditor.stopCellEditing()
      super.columnMarginChanged(e);
  }

  override def tableChanged(e:TableModelEvent) {
    //See note on declaration of responding
    if (responding == null) {
       responding = new AtomicBoolean(false);
    }
    responding.set(true);
    super.tableChanged(e);
    responding.set(false);
  }

  def isRespondingToChange = {
    //See note on declaration of responding
    if (responding == null) {
       responding = new AtomicBoolean(false);
    }
    responding.get
  }

}