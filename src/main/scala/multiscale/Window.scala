package multiscale

import scalafx.Includes._
import scalafx.application.JFXApp
import scalafx.scene._
import scalafx.scene.paint._
import scalafx.scene.layout._
import scalafx.application.JFXApp.PrimaryStage
import scalafx.geometry._
import scalafx.scene.canvas.Canvas
import scalafx.scene.control._
import scalafx.event.ActionEvent

object Window extends JFXApp {

  val controls = new VBox {
    spacing = 10
    padding = Insets(10, 0, 0, 0)
    prefWidth = 160

    val reset = new Button("Reset") {
      prefWidth_=(150)
      onAction = { e: ActionEvent =>
        CA.clear
      }
    }

    val randomizeCA = new Button("Randomize cells") {
      prefWidth = 150
      onAction = { e: ActionEvent =>
        MonteCarlo.randomize
      }
    }

    val nextItMC = new Button("Next MC It.") {
      prefWidth = 150
      onAction = { e: ActionEvent =>
        MonteCarlo.runSimulation
      }
    }

    val showEnergy = new Button("Energy") {
      prefWidth = 150
      onAction = { e: ActionEvent =>
        MonteCarlo.showEnergy
      }
    }

    val nucleation = new Button("Nucleation") {
      prefWidth = 150
      onAction = { e: ActionEvent =>
        MonteCarlo.constantNucleation
      }
    }

    val recrystalization = new Button("Recrystalization") {
      prefWidth = 150
      onAction = { e: ActionEvent =>
        MonteCarlo.runRecrystalization
      }
    }

    val energyToggle = new ToggleGroup()

    val energyToggleOn = new RadioButton {
      maxWidth = 200
      maxHeight = 50
      text = "On"
      toggleGroup = energyToggle
      onAction = { e: ActionEvent =>
        MonteCarlo.showEnergy
      }
    }
    val energyToggleOff = new RadioButton {
      maxWidth = 200
      maxHeight = 50
      text = "Off"
      selected = true
      toggleGroup = energyToggle
      onAction = { e: ActionEvent =>
        MonteCarlo.showCA
      }
    }

    content += reset
    content += randomizeCA
    content += nextItMC
    content += new Label("Energy:")
    content += energyToggleOn
    content += energyToggleOff
    content += nucleation
    content += recrystalization

  }

  val canvas = new Canvas(CA.imgWidth, CA.imgHeight) {
    this.graphicsContext2D.fill = Color.WHITE
    this.graphicsContext2D.fillRect(0, 0, CA.imgWidth, CA.imgHeight)
    CA.gc = graphicsContext2D
  }
  canvas.styleClass.add("canvas")

  val canvasBox = new BorderPane {
    padding = Insets(0, 0, 0, 10)
    center = canvas
  }

  val mainContent = new BorderPane {
    padding = Insets(10)
    left = controls
    center = canvasBox
  }
  mainContent.styleClass.add("mainContent")

  stage = new PrimaryStage {
    title = "CA"
    scene = new Scene {
      stylesheets add Window.getClass.getResource("/layout.css").toExternalForm
      content = mainContent
      resizable = false
    }
  }
}