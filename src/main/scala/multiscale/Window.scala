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
import scalafx.scene.input.InputEvent
import scalafx.collections.ObservableBuffer
import scala.util.Random

object Window extends JFXApp {

  val controls:  VBox {val reset: Button; val randomizeCA: Button; val nextItMC: Button; val showEnergy: Button; val nucleation: Button; val recrystalization: Button; val energyToggle: ToggleGroup; val energyToggleOn: RadioButton; val energyToggleOff: RadioButton; val monteCarloStep: TextField; val monteCarloJgb: TextField; val monteCarloOmega: TextField; val monteCarloRecStep: TextField; val monteCarloRecJgb: TextField; val energyDistributionType: ChoiceBox[String]; val nucleationType: ChoiceBox[String]; val nucleationRate: ChoiceBox[String]; val nucleationFactor: TextField; val nucleationStart: TextField; val nucleationStep: TextField} = new VBox {
    spacing = 10
    padding = Insets(10, 0, 0, 0)
    prefWidth = 160

    val reset = new Button("Reset") {
      prefWidth = 150
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

    val nextItMC = new Button("Run Monte Carlo") {
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
        CA.refreshCA
      }
    }

    val recrystalization = new Button("Recrystallization") {
      prefWidth = 150
      onAction = { e: ActionEvent =>
        MonteCarlo.runRecrystalization
      }
    }

    val energyToggle = new ToggleGroup()

    val energyToggleOn = new RadioButton {
      prefWidth = 150
      text = "On"
      toggleGroup = energyToggle
      onAction = { e: ActionEvent =>
        MonteCarlo.showEnergy
      }
    }
    val energyToggleOff = new RadioButton {
      prefWidth = 150
      text = "Off"
      selected = true
      toggleGroup = energyToggle
      onAction = { e: ActionEvent =>
        MonteCarlo.showCA
      }
    }

    val monteCarloStep = new TextField {
      prefWidth = 150
      maxWidth = 150
      text = "1"
      onAction = { e: ActionEvent =>
        MonteCarlo.steps = this.text.value.toInt * CA.cellsHorizontal * CA.cellsVertical
      }
      focused.onChange((_, _, newValue) => {
        MonteCarlo.steps = this.text.value.toInt * CA.cellsHorizontal * CA.cellsVertical
      })
    }
    val monteCarloJgb = new TextField {
      prefWidth = 150
      maxWidth = 150
      text = "1"
      onAction = { e: ActionEvent =>
        MonteCarlo.Jgb = this.text.value.toDouble
      }
      focused.onChange((_, _, newValue) => {
        MonteCarlo.Jgb = this.text.value.toDouble
      })
    }
    val monteCarloOmega = new TextField {
      prefWidth = 150
      maxWidth = 150
      text = "10"
      onAction = { e: ActionEvent =>
        MonteCarlo.Omega = this.text.value.toInt
      }
      focused.onChange((_, _, newValue) => {
        MonteCarlo.Omega = this.text.value.toInt
      })
    }
    val monteCarloRecStep = new TextField {
      prefWidth = 150
      maxWidth = 150
      text = "1"
      onAction = { e: ActionEvent =>
        MonteCarlo.recSteps = this.text.value.toInt * CA.cellsHorizontal * CA.cellsVertical
      }
      focused.onChange((_, _, newValue) => {
        MonteCarlo.recSteps = this.text.value.toInt * CA.cellsHorizontal * CA.cellsVertical
      })
    }

    val monteCarloRecJgb = new TextField {
      prefWidth = 150
      maxWidth = 150
      text = "0.5"
      onAction = { e: ActionEvent =>
        MonteCarlo.recJgb = this.text.value.toDouble
      }
      focused.onChange((_, _, newValue) => {
        MonteCarlo.recJgb = this.text.value.toDouble
      })
    }

    val energyDistributionType = new ChoiceBox[String] {
      items = ObservableBuffer("Homogeneous", "Heterogeneous")
      selectionModel().selectFirst()
      prefWidth_=(150)
      selectionModel().selectedItem.onChange({
        selectionModel().selectedItem.value match {
          case "Homogeneous" => MonteCarlo.H = Array.fill(CA.cellsHorizontal,CA.cellsVertical)(4)
          case "Heterogeneous" => MonteCarlo.H = Array.fill(CA.cellsHorizontal, CA.cellsVertical)(new Random().nextInt(5))
        }


      })
    }

    val nucleationType = new ChoiceBox[String] {
      items = ObservableBuffer("Homogeneous", "Heterogeneous")
      selectionModel().selectFirst()
      prefWidth_=(150)
    }

    val nucleationRate = new ChoiceBox[String] {
      items = ObservableBuffer("Constant", "Increasing", "Decreasing", "All at once")
      selectionModel().selectFirst()
      prefWidth_=(150)
    }

    val nucleationFactor = new TextField {
      prefWidth = 150
      maxWidth = 150
      text = "10"
      onAction = { e: ActionEvent =>
        MonteCarlo.nucleationFactor = this.text.value.toInt
      }
      focused.onChange((_, _, newValue) => {
        MonteCarlo.nucleationFactor = this.text.value.toInt
      })
    }

    val nucleationStart = new TextField {
      prefWidth = 150
      maxWidth = 150
      text = "100"
      onAction = { e: ActionEvent =>
        MonteCarlo.nucleationStart = this.text.value.toInt
      }
      focused.onChange((_, _, newValue) => {
        MonteCarlo.nucleationStart = this.text.value.toInt
      })
    }

    val nucleationStep = new TextField {
      prefWidth = 150
      maxWidth = 150
      text = "1"
      onAction = { e: ActionEvent =>
        MonteCarlo.nucleationStep = this.text.value.toInt * CA.cellsVertical * CA.cellsHorizontal
      }
      focused.onChange((_, _, newValue) => {
        MonteCarlo.nucleationStep = this.text.value.toInt * CA.cellsVertical * CA.cellsHorizontal
      })
    }



    content = IndexedSeq(
      reset,
      randomizeCA,
      new Label("Monte Carlo states #"),
      monteCarloOmega,
      new Label("Monte Carlo Step"),
      monteCarloStep,
      new Label("Monte Carlo J_gb"),
      monteCarloJgb, nextItMC,
      new Label("Energy distribution type"),
      energyDistributionType,
      new Label("Energy:"),
      energyToggleOn,
      energyToggleOff,
      new Label("Nucleation type"),
      nucleationType,
      new Label("Nucleation rate"),
      nucleationRate,
      new Label("Base nucleons #"),
      nucleationStart,
      new Label("Nucleation factor"),
      nucleationFactor,
      new Label("Steps between nucleation"),
      nucleationStep,
      nucleation,
      new Label("Recrystallization Step"),
      monteCarloRecStep,
      new Label("Recrystallization J_gb"),
      monteCarloRecJgb,
      recrystalization
    )


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