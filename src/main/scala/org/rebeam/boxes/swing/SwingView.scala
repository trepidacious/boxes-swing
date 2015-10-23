package org.rebeam.boxes.swing

import scala.collection._
import java.awt.event.{FocusEvent, FocusListener, ActionEvent, ActionListener}
import javax.swing.JToggleButton.ToggleButtonModel
import math.Numeric
import java.util.concurrent.atomic.AtomicBoolean
import com.explodingpixels.painter.MacWidgetsPainter
import java.awt.geom.Arc2D
import javax.swing.JSpinner.DefaultEditor
import java.text.ParseException
import javax.swing.plaf.metal.MetalLookAndFeel
import javax.swing.border.{EmptyBorder, MatteBorder}
import javax.swing.table.{TableModel, TableCellRenderer, TableCellEditor, AbstractTableModel}
import javax.swing.event.{TableModelEvent, ChangeEvent, TableColumnModelEvent}
import org.rebeam.boxes.core.util.{GConverter, OptionTConverter, TConverter, CoalescingResponder, Sequence}
import javax.swing.{JPanel, ScrollPaneConstants, JTable, JSpinner, SpinnerModel, SpinnerNumberModel, JProgressBar, JSlider, BoundedRangeModel, DefaultBoundedRangeModel, SwingConstants, Icon, JTextArea, JScrollPane, JTextField, JLabel, JComponent, ImageIcon, UIManager, SwingUtilities}
import com.explodingpixels.swingx.EPPanel
import java.awt.{BorderLayout, AlphaComposite, Dimension, BasicStroke, RenderingHints, Graphics2D, Color, Component}
import org.rebeam.boxes.swing.icons.IconFactory
import java.text.DecimalFormat
import java.util.concurrent.Executor
import java.awt.LayoutManager

import org.rebeam.boxes.core._
import org.rebeam.boxes.core.util._
import BoxUtils._
import BoxTypes._
import BoxScriptImports._

import java.util.concurrent.{ExecutorService, Executors, Executor}

import scala.language.implicitConversions

// object SwingViewImplicits {
//   implicit def varToBooleanView(v: Var[Boolean]) = BooleanView(v)
//   implicit def varToNumberView[N](v: Var[N])(implicit n: Numeric[N], nc: NumericClass[N]) = NumberView[N](v)
//   implicit def varToStringView(v: Var[String]) = StringView(v)

//   implicit def optionVarToBooleanView(v: Var[Option[Boolean]]) = BooleanOptionView(v)
//   implicit def optionVarToNumberView[N](v: Var[Option[N]])(implicit n: Numeric[N], nc: NumericClass[N]) = NumberOptionView[N](v)
//   implicit def optionVarToStringView(v: Var[Option[String]]) = StringOptionView(v)
// }

object SwingView {

  val defaultExecutorPoolSize = 8
  val defaultThreadFactory = DaemonThreadFactory()
  lazy val defaultExecutor: Executor = Executors.newFixedThreadPool(defaultExecutorPoolSize, defaultThreadFactory)

  val defaultDecimalFormat = new DecimalFormat("0.#")
  val viewToUpdates = new mutable.WeakHashMap[Any, mutable.ArrayBuffer[() => Unit]]()
  //TODO consider using smaller intervals than default. 5, 10 makes graph use very smooth on a good PC. 
  val responder = new CoalescingResponder(respond)
  val lock = new Object()
  
  def format(d: Double) = defaultDecimalFormat.format(d)

  def icon(name: String) = IconFactory.icon(name)

  val wrench = icon("Wrench")



  def observer[A](v: Any, script: BoxScript[A])(effect: A => Unit): Observer = Observer(
    script, 

    //Run effect in the swing thread, using replace to eliminate pointless updates
    (a: A) => replaceUpdate(v, effect(a)), 

    //Run script on our own default executor
    defaultExecutor,  

    //Only most recent revisions
    true
  )

  def addUpdate(v: Any, update: => Unit) = {
    lock.synchronized{
      viewToUpdates.get(v) match {
        case None => viewToUpdates.put(v, mutable.ArrayBuffer(() => update))
        case Some(list) => list.append(() => update)
      }
      responder.request
    }
  }

  def replaceUpdate(v: Any, update: => Unit) = {
    lock.synchronized{
      viewToUpdates.put(v, mutable.ArrayBuffer(() => update))
      responder.request
    }
  }

  private def respond() = {
    SwingUtilities.invokeLater(new Runnable() {
      override def run = {
        while(
          popUpdates match {
            case Some(updates) => {
              for {
                update <- updates
              } update.apply
              true
            }
            case None => false
          }
        ) {}
      }
    })
  }

  /**
   * If there are any updates stored in map, get a list of updates
   * for some key, remove them from the map, and return them.
   * If there are no updates left (no keys), then return None
   * This is synchronized, so updates can't be added as they are being
   * retrieved
   */
  private def popUpdates = {
    lock.synchronized{
      val keysIt = viewToUpdates.keysIterator;
      if (keysIt.hasNext) {
        val r = viewToUpdates.remove(keysIt.next)
        if (r == None) throw new RuntimeException("Got None for a key in viewToUpdates")
        r
      } else {
        None
      }
    }
  }

  def nimbus() {
    try {
      for (info <- UIManager.getInstalledLookAndFeels()) {
        if ("Nimbus".equals(info.getName())) {
          UIManager.setLookAndFeel(info.getClassName())
          UIManager.put("control", background)
          UIManager.put("nimbusSelectionBackground", selectionColor);
          UIManager.put("Table.alternateRowColor", alternateBackgroundColor)
          UIManager.put("Table.backgroundColor", selectedTextColor)
          UIManager.put("Table.selectionForeground", selectedTextColor)
          UIManager.put("Table.selectionBackground", selectionColor)
          UIManager.put("Table.focusCellHighlightBorder", new MatteBorder(1, 1, 1, 1, selectionColor.darker.darker))
          UIManager.put("CheckBox.icon", IconFactory.icon("Checkbox"))
        }
      }
    } catch {
      case _: Throwable => {}
    }
  }
  
//  def nimbox() {
//    try {
//      UIManager.setLookAndFeel( new MetalLookAndFeel() )
//      UIManager.put("Table.alternateRowColor", alternateBackgroundColor)
//      UIManager.put("Table.backgroundColor", selectedTextColor)
//      UIManager.put("Table.selectionForeground", selectedTextColor)
//      UIManager.put("Table.selectionBackground", selectionColor)
//      UIManager.put("Table.focusCellHighlightBorder", new MatteBorder(1, 1, 1, 1, selectionColor.darker.darker))
//    }
//    catch {
//      case _:Throwable => {}
//    }
//  }

  val background = new Color(240, 240, 240)
  val dividingColor = new Color(150, 150, 150)
  val unimportantTextColor = new Color(150, 150, 150)
  val alternateBackgroundColor = new Color(240, 240, 240)
  val selectionColor = new Color(120, 144, 161)
  val selectedTextColor = Color.white
  val textColor = new Color(50, 50, 50)
  val textUnderlightColor = new Color(255, 255, 255, 160)
  val shadedBoxColor = new Color(0,0,0,0.6f)

  def clip(value: Int, min: Int, max: Int) = {
    if (value < min) min
    else if (value > max) max
    else value
  }

  def transparentColor(c: Color, factor: Double) = {
    new Color(  c.getRed,
                c.getGreen,
                c.getBlue,
                clip((c.getAlpha() * factor).asInstanceOf[Int], 0, 255))
  }

  def graphicsForEnabledState(g: Graphics2D, e: Boolean) {
    if (!e) g.setComposite(AlphaComposite.getInstance(AlphaComposite.SRC_OVER, 0.5f))
  }

  def later(f: => Unit) {
    SwingUtilities.invokeLater(new Runnable(){
      override def run() = f
    })
  }
  
  //TODO add this
//  val iconFactory = new ResourceIconFactory()
}

trait SwingView {
  def component(): JComponent

  private def observer[A](script: BoxScript[A])(effect: A => Unit): Observer = SwingView.observer(this, script)(effect)
}

//Special versions of components that link back to the SwingView using them,
//so that if users only retain the component, they still also retain the SwingView.
class LinkingJLabel(val sv: SwingView) extends Label {}

class LinkingTextEPPanel(val sv: SwingView, contents: Component) extends EPPanel {
  setBackgroundPainter(new TextComponentPainter())
  setBorder(new EmptyBorder(7,8,4,4))
  setLayout(new BorderLayout())
  add(contents)
}

//Special versions of components that link back to the SwingView using them,
//so that if users only retain the component, they still also retain the SwingView.
class LinkingJScrollPane(val sv: SwingView, contents: Component) extends JScrollPane(contents)

class LinkingTextJScrollPane(val sv: SwingView, contents: Component) extends JScrollPane(contents) {
  BoxesScrollBarUI.applyTo(this, plain = true)
  val lowerRightCorner = new EPPanel()
  lowerRightCorner.setBackgroundPainter(new WhitePainter())
  setCorner(ScrollPaneConstants.LOWER_RIGHT_CORNER, lowerRightCorner)
  setHorizontalScrollBarPolicy(ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED)
  setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED)
}


class LinkingJTextField(val sv: SwingView) extends JTextField {
  BoxesTextFieldUI(this)
}

class BoxesJTextArea(r: Int, c: Int) extends JTextArea(r, c) {
  BoxesTextAreaUI(this)
}

object Label {
  def apply(text: String, icon: Option[Icon] = None, horizontalAlignment: Int = SwingConstants.LEFT) = new Label(text, icon, horizontalAlignment)
}

class Label(text: String="", icon: Option[Icon] = None, horizontalAlignment: Int = SwingConstants.LEFT) extends JLabel(text, icon.getOrElse(null), horizontalAlignment) {
  {
    setBorder(new EmptyBorder(7, 2, 6, 2))
  }
}

class LinkingSlideCheckButton(val sv: SwingView) extends SlideCheckButton

class LinkingTabButton(val sv: SwingView) extends TabButton

class LinkingJCheckBox(val sv: SwingView) extends BoxesCheckBox

class LinkingJRadioButton(val sv: SwingView) extends BoxesRadioButton

class LinkingJToggleButton(val sv: SwingView) extends SwingToggleButton

class LinkingToolbarToggleButton(val sv: SwingView) extends SwingBarToggleButton



class LinkingJSlider(val sv: SwingView, brm: BoundedRangeModel) extends JSlider(brm) {
  {
    setUI(new BoxesSliderUI(this))
  }
}
class LinkingJProgressBar(val sv: SwingView, brm: BoundedRangeModel) extends JProgressBar(brm) {
  {
    setUI(new BoxesProgressUI())
  }
}

object PiePainter {

  val defaultFill = SwingView.selectionColor
  val defaultOutline = Color.white

  def apply(border: Int = 3, dotRadius: Int = 2, fill: Color = defaultFill, outline: Color = defaultOutline, justDot: Boolean = false) = new PiePainter(border, dotRadius, fill, outline, justDot)
}

class PiePainter(val border: Int, val dotRadius: Int, val fill: Color, val outline: Color, val justDot: Boolean) {

  def paint(g: Graphics2D, n: Double, w: Int, h: Int, alpha: Double = 1) {

    val oldAA = g.getRenderingHint(RenderingHints.KEY_ANTIALIASING)
    val oldFM = g.getRenderingHint(RenderingHints.KEY_FRACTIONALMETRICS)

    g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
    g.setRenderingHint(RenderingHints.KEY_FRACTIONALMETRICS, RenderingHints.VALUE_FRACTIONALMETRICS_ON)

    val arcAngle = - (n * 360).asInstanceOf[Int]

    val size = math.min(w, h)

    val circleDiameter = size - 2 * (dotRadius + border)

    g.setStroke(new BasicStroke(dotRadius * 2 + 3))
    g.setPaint(SwingView.transparentColor(outline, alpha))
    g.drawOval(border + dotRadius, border + dotRadius, circleDiameter, circleDiameter)

    if (justDot) {
      g.setPaint(SwingView.transparentColor(fill, alpha))
      val x = (size/2 + math.cos(-(arcAngle+90)/360d * math.Pi * 2) * circleDiameter/2 + 1).asInstanceOf[Int]
      val y = (size/2 + math.sin(-(arcAngle+90)/360d * math.Pi * 2) * circleDiameter/2 + 1).asInstanceOf[Int]

      g.fillOval(x - dotRadius, y - dotRadius, dotRadius*2, dotRadius*2)

    } else {
      val arc = new Arc2D.Double(0, 0, size, size, 90, arcAngle, Arc2D.PIE)

      val clip = g.getClip()
      g.setPaint(SwingView.transparentColor(fill, alpha))
      g.setClip(arc)
      g.setStroke(new BasicStroke(dotRadius * 2))
      g.drawOval(border + dotRadius, border + dotRadius, circleDiameter, circleDiameter)
      g.setClip(clip)
    }

    g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, oldAA)
    g.setRenderingHint(RenderingHints.KEY_FRACTIONALMETRICS, oldFM)
  }
}

class LinkingEPPanel(val sv: SwingView) extends EPPanel {}

class LinkingJSpinner(val sv: SwingView, m: SpinnerModel) extends JSpinner(m) {
  {
    this.setUI(new BoxesSpinnerUI())
    this.setMinimumSize(new Dimension(60, 28))
    this.setPreferredSize(new Dimension(60, 28))
  }
}

object BoxesScrollPane {
  def apply(component: JComponent, horizontal: Boolean = false, vertical: Boolean = true) = {
    val scroll = new JScrollPane(component)
    BoxesScrollBarUI.applyTo(scroll, horizontal = horizontal, vertical = vertical)
    scroll
  }
  def horizontal(component: JComponent) = {
    val scroll = new JScrollPane(component)
    BoxesScrollBarUI.applyTo(scroll, new DotModel, new DotModel, true, false)
    scroll
  }
  def vertical(component: JComponent) = {
    val scroll = new JScrollPane(component)
    BoxesScrollBarUI.applyTo(scroll, new DotModel, new DotModel, false, true)
    scroll
  }
}

class LinkingJPanel(val view: AnyRef, layout: LayoutManager) extends JPanel(layout)




