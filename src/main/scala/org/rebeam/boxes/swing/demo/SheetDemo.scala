package org.rebeam.boxes.transact.swing.demo

import javax.swing.JFrame
import javax.swing.JPanel
import javax.swing.JButton
import javax.swing.AbstractAction
import java.awt.event.ActionEvent
import org.rebeam.boxes.swing.views._
import org.rebeam.boxes.swing.layout._
import javax.swing.SwingUtilities
import org.rebeam.boxes.swing._
import java.text.DecimalFormat
import org.rebeam.boxes.core._
import BoxUtils._
import BoxTypes._
import BoxScriptImports._


object SheetDemoApp extends App {

  case class Sine(
    name: Box[String], 
    phase: Box[Double], 
    amplitude: Box[Double], 
    enabled: Box[Boolean], 
    points: Box[Boolean], 
    description: Box[String],
    lineWidth: Box[Int]
  ) 

  object Sine {
    def default: BoxScript[Sine] = default("", 0d, 1d, true, false, "Default Description\nCan have multiple lines", 1)

    def default(
      name: String, 
      phase: Double, 
      amplitude: Double, 
      enabled: Boolean, 
      points: Boolean, 
      description: String,
      lineWidth: Int
    ): BoxScript[Sine] = for {
      n <- create(name)
      ph <- create(phase)
      a <- create(amplitude)
      e <- create(enabled)
      p <- create(points)
      d <- create(description)
      lw <- create(lineWidth)
    } yield Sine(n, ph, a, e, p, d, lw)
  }
  
def propertiesSheet(s: Sine) = {

  val nameView = StringView(s.name)
  val nameLabelView = LabelView(s.name)
  
  val amplitudeView = NumberView(s.amplitude)
  val amplitudePieView = PieView(s.amplitude, s.amplitude)
  val phaseView = NumberView(s.phase)
  val enabledView = BooleanView(s.enabled)
  val descriptionView = StringView(s.description, true)
  val pointsView = BooleanView(s.points)
  val lineWidthView = NumberView(s.lineWidth)
  val lineWidthRangeView = RangeView(s.lineWidth, 1, 10, false)
  val lineWidthProgressView = RangeView(s.lineWidth, 1, 10, true)

  val sheet = SheetBuilder()
  sheet
    .separator("Edit Sine")
    .view("Name", nameView)
    .view("Name (label)", nameLabelView)
    .view("Amplitude", amplitudeView)
    .view("Amplitude (pie/10)", amplitudePieView)
    .view("Phase", phaseView)
    .view("Enabled", enabledView)
    .view("Points", pointsView)
    .view("Line width", lineWidthView)
    .view("Line width", lineWidthRangeView)
    .view("Line width", lineWidthProgressView)
    .view("Description", descriptionView, true)
    .panel

}

  SwingView.later {
    
    SwingView.nimbus
    
    val s = atomic{Sine.default}

    val properties = propertiesSheet(s)
    val properties2 = propertiesSheet(s)

    val frame = new JFrame("Transact Swing Sheet Demo")
    
    frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)

    val panel = new JPanel()
    panel.add(properties);
    panel.add(properties2);

    frame.add(panel);
    frame.pack()
    frame.setVisible(true)
  }

}