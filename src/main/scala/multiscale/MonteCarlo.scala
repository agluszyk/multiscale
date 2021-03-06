package multiscale

import scalafx.scene.paint.Color
import scalafx.application.Platform
import scala.math._
import scala.util.Random
import scala.util.Failure
import scala.collection.mutable.Map
import scala.collection.mutable.HashSet

object MonteCarlo {
  val ca = CA.caArray
  val rand = new Random()
  var Omega = 10
  var steps = CA.cellsHorizontal * CA.cellsVertical
  var recSteps = 1
  var recJgb = 0.5
  var Jgb: Double = 1
  var H = Array.fill(CA.cellsHorizontal, CA.cellsVertical)(4)
  var nucleationFactor = 10
  var nucleationStart = 100
  var nucleationStep = 1  * CA.cellsVertical * CA.cellsHorizontal
  var currentRecStep = 0
  var nucleationPercentOnBoundaries = 90
  val cellsOnBoundaries = scala.collection.mutable.ArrayBuffer[(Int, Int)]()

  var possibleStates = {
    for (i <- 0 until 100) yield {
      Color(random, random, random, 1)
    }

  }

  def randomize() = {
    (0 until CA.cellsHorizontal).par.foreach {
      i =>
        (0 until CA.cellsVertical).par.foreach {
          j =>
            CA.caArray(i)(j) = possibleStates(rand.nextInt(Omega))
        }
    }
    CA.refreshCA
  }

  def showEnergy = {
    (0 until CA.cellsHorizontal).toList.foreach {
      i =>
        (0 until CA.cellsVertical).toList.foreach {
          j =>
            val cellH = H(i)(j) match {
              case 0 => Color(0, 0, 1, 1)
              case 1 => Color(0.2, 0, 0.8, 1)
              case 2 => Color(0.5, 0, 0.5, 1)
              case 3 => Color(0.8, 0, 0.2, 1)
              case 4 => Color(1, 0, 0, 1)
            }
            CA.gc.fill = cellH
            CA.gc.fillRect(i * CA.cellSize, j * CA.cellSize, CA.cellSize, CA.cellSize)
        }
    }
  }

  def showCA = {

    CA.gc.fill = Color.WHITE
    CA.gc.fillRect(0, 0, CA.imgWidth, CA.imgHeight)

    for (i <- 0 until CA.cellsHorizontal) {
      for (j <- 0 until CA.cellsVertical) {
        CA.gc.fill = ca(i)(j)
        CA.gc.fillRect(i * CA.cellSize, j * CA.cellSize, CA.cellSize, CA.cellSize)
      }
    }

    for {i <- 0 until CA.cellsHorizontal
         j <- 0 until CA.cellsVertical
         if CA.srxArray(i)(j) != Color.WHITE} {
      CA.gc.fill = ca(i)(j)
      CA.gc.fillRect(i * CA.cellSize, j * CA.cellSize, CA.cellSize, CA.cellSize)
    }
  }

  def runSimulation = {
    val timeBeg = System.currentTimeMillis()
    (0 until steps).par.foreach {
      s =>
      // 1. choose random cell
        val x = rand.nextInt(CA.cellsHorizontal)
        val y = rand.nextInt(CA.cellsVertical)
        // Optimization #1
        val nbs = CA.getNb(x, y)
        var isInGrain = true
        nbs.foreach {
          nb =>
            if (ca(nb._1)(nb._2) != ca(x)(y)) {
              isInGrain = false
            }
        }

        if (!isInGrain) {

          // 2. calculate grain boundary energy
          var sum = 0
          nbs.foreach {
            nb =>
              if (!(ca(x)(y) == ca(nb._1)(nb._2))) {
                sum = sum + 1
              }
          }
          val Egb = Jgb * sum
          // 3. Change state to orientation from Omega - 1 => Optimized to cells from sourrounding
          val nbPossibleStates = for {
            nb <- nbs
            if ca(x)(y) != ca(nb._1)(nb._2)
          } yield {
            ca(nb._1)(nb._2)
          }

          var nextOrientation = ca(x)(y)
          if (nbPossibleStates.size - 1 > 1)
            nextOrientation = nbPossibleStates(rand.nextInt(nbPossibleStates.size))

          // 4. Recalculate energy
          sum = 0
          nbs.foreach {
            nb =>
              if (!(nextOrientation == ca(nb._1)(nb._2))) {
                sum = sum + 1
              }
          }
          val Egb_afterChange = Jgb * sum
          // 5. Compare and switch

          if (Egb_afterChange <= Egb) {
            CA.addArrayCell(x, y, nextOrientation)
          }
        }
    }
    CA.refreshCA
    System.out.println("Elapsed time: "+(System.currentTimeMillis() - timeBeg) + " ms")
  }

  // Old recrys. function
  /*def runRecrystalization: Unit = {

    //create cells on boundaries seq
    if (currentRecStep == 0) {
      for {i <- 0 until CA.cellsHorizontal
           j <- 0 until CA.cellsVertical
           if CA.getNb(i, j).exists(e => CA.caArray(e._1)(e._2) != CA.caArray(i)(j))
      } {
        cellsOnBoundaries += ((i, j))
      }
    }

    (0 until recSteps).par.foreach {
      s =>

      //create new nucleons in this step
        if (s == 0) {
          Window.controls.nucleationRate.selectionModel().getSelectedItem match {
            case "Constant" => {
              currentRecStep += 1
              constantNucleation
            }
            case "Increasing" => {
              currentRecStep += 1
              increasingNucleation
            }
            case "Decreasing" => {
              currentRecStep += 1
              decreasingNucleation
            }
            case "All at once" => {
              currentRecStep += 1
              if (currentRecStep == 1) {
                constantNucleation
              }
            }
          }
        }

        // 1. choose random cell
        val x = rand.nextInt(CA.cellsHorizontal)
        val y = rand.nextInt(CA.cellsVertical)
        // Optimization #1
        val nbs = CA.getNb(x, y)

        var isInGrain = true
        var isNbRecrystalized = false

        /*var isAlreadyRecrystalized = false

        if (recrystalizedCells.exists(e => e._1 == x && e._2 == y)) {
          isNbRecrystalized = true
        }*/

        nbs.foreach {
          nb =>
            if (CA.srxArray(nb._1)(nb._2) != CA.srxArray(x)(y)) {
              isInGrain = false
            }
            if (CA.srxArray(nb._1)(nb._2) != Color.WHITE) {
              isNbRecrystalized = true
            }
        }

        if (!isInGrain && isNbRecrystalized /*&& !isAlreadyRecrystalized*/ ) {

          // 2. calculate grain boundary energy
          var sum = 0
          nbs.foreach {
            nb =>
              if (!(CA.srxArray(x)(y) == CA.srxArray(nb._1)(nb._2))) {
                sum = sum + 1
              }
          }
          val Egb = Jgb * sum
          // 3. Change state to orientation from Omega - 1 => Optimized to cells from sourrounding
          val nbPossibleStates = for {
            nb <- nbs
            if (CA.srxArray(x)(y) != CA.srxArray(nb._1)(nb._2))
            if (CA.srxArray(nb._1)(nb._2) != Color.WHITE)
          } yield {
            CA.srxArray(nb._1)(nb._2)
          }

          var nextOrientation = CA.srxArray(x)(y)
          if (!nbPossibleStates.isEmpty)
            nextOrientation = nbPossibleStates(rand.nextInt(nbPossibleStates.size))

          // 4. Recalculate energy
          sum = 0
          nbs.foreach {
            nb =>
              if (!(nextOrientation == CA.srxArray(nb._1)(nb._2))) {
                sum = sum + 1
              }
          }
          val Egb_afterChange = recJgb * sum
          // 5. Compare and switch

          if (Egb_afterChange <= Egb) {
            H(x)(y) = 0
            CA.srxAddArrayCell(x, y, nextOrientation)
          }
        }
    }

    /*  for (i <- 0 until CA.cellsHorizontal) {
        for (j <- 0 until CA.cellsVertical) {
          CA.gc.fill = ca(i)(j)
          CA.gc.fillRect(i * CA.cellSize, j * CA.cellSize, CA.cellSize, CA.cellSize)
        }
      }*/
    CA.refreshCA
    //     }
    //recrystalizedCells
  }*/


  def runSRX: Unit = {
    val timeBeg = System.currentTimeMillis()

    //create cells on boundaries seq
    if (currentRecStep == 0) {
      for {i <- 0 until CA.cellsHorizontal
           j <- 0 until CA.cellsVertical
           if CA.getNb(i, j).exists(e => CA.caArray(e._1)(e._2) != CA.caArray(i)(j))
      } {
        cellsOnBoundaries += ((i, j))
      }
    }

    (0 until recSteps).par.foreach {
      s =>

      //create new nucleons in this step
        if (s % nucleationStep == 0) {
          Window.controls.nucleationRate.selectionModel().getSelectedItem match {
            case "Constant" => {
              currentRecStep += 1
              constantNucleation
            }
            case "Increasing" => {
              currentRecStep += 1
              increasingNucleation
            }
            case "Decreasing" => {
              currentRecStep += 1
              decreasingNucleation
            }
            case "All at once" => {
              currentRecStep += 1
              if (currentRecStep == 1) {
                constantNucleation
              }
            }
          }
        }

        // 1. choose random cell
        val x = rand.nextInt(CA.cellsHorizontal)
        val y = rand.nextInt(CA.cellsVertical)
        // Optimization #1
        val nbs = CA.getNb(x, y)

        var isInGrain = true
        var isNbRecrystalized = false


        nbs.foreach {
          nb =>
            if (CA.srxArray(nb._1)(nb._2) != CA.srxArray(x)(y)) {
              isInGrain = false
            }
            if (CA.srxArray(nb._1)(nb._2) != Color.WHITE) {
              isNbRecrystalized = true
            }
        }

        if (!isInGrain && isNbRecrystalized) {

          // 2. calculate grain boundary energy
          var sum = 0
          nbs.foreach {
            nb =>
              if (!(CA.srxArray(x)(y) == CA.srxArray(nb._1)(nb._2))) {
                sum = sum + 1
              }
          }
          val Egb = (recJgb * sum) + H(x)(y)
          // 3. Change state to orientation from Omega - 1 => Optimized to cells from sourrounding
          val nbPossibleStates = for {
            nb <- nbs
            if (CA.srxArray(x)(y) != CA.srxArray(nb._1)(nb._2))
            if (CA.srxArray(nb._1)(nb._2) != Color.WHITE)
          } yield {
            CA.srxArray(nb._1)(nb._2)
          }

          var nextOrientation = CA.srxArray(x)(y)
          if (!nbPossibleStates.isEmpty)
            nextOrientation = nbPossibleStates(rand.nextInt(nbPossibleStates.size))

          // 4. Recalculate energy
          sum = 0
          nbs.foreach {
            nb =>
              if (!(nextOrientation == CA.srxArray(nb._1)(nb._2))) {
                sum = sum + 1
              }
          }
          val Egb_afterChange = recJgb * sum
          // 5. Compare and switch

          if (Egb_afterChange <= Egb) {
            H(x)(y) = 0
            CA.srxAddArrayCell(x, y, nextOrientation)
          }
        }
    }
    CA.refreshCA
    System.out.println("Elapsed time: "+(System.currentTimeMillis()-timeBeg) + " ms")
  }


  def constantNucleation = {
    for (i <- 0 until Window.controls.nucleationFactor.text.value.toInt) {
      val coords = Window.controls.nucleationType.selectionModel().getSelectedItem match {
        case "Homogeneous" => {
          val px = (abs(rand.nextInt) % (CA.cellsHorizontal - 2)) + 1
          val py = (abs(rand.nextInt) % (CA.cellsVertical - 2)) + 1
          cellsOnBoundaries += ((px, py))
          (px, py)
        }
        case "Heterogeneous" => {
          val los = rand.nextInt(100) + 1
            val cell = if (los < nucleationPercentOnBoundaries)
           cellsOnBoundaries(rand.nextInt(cellsOnBoundaries.size))
         else
           ((abs(rand.nextInt) % (CA.cellsHorizontal - 2)) + 1,(abs(rand.nextInt) % (CA.cellsVertical - 2)) + 1)
            CA.getNb(cell._1, cell._2).foreach(nb =>
              cellsOnBoundaries += ((nb._1, nb._2)))
            (cell._1, cell._2)
        }
      }
      CA.srxAddArrayCell(coords._1, coords._2, possibleStates(rand.nextInt(Omega)))
      H(coords._1)(coords._2) = 0
    }
  }

  def increasingNucleation = {
    for (i <- 0 until Window.controls.nucleationStart.text.value.toInt + (currentRecStep * Window.controls.nucleationFactor.text.value.toInt)) {
      val coords = Window.controls.nucleationType.selectionModel().getSelectedItem match {
        case "Homogeneous" => {
          val px = (abs(rand.nextInt) % (CA.cellsHorizontal - 2)) + 1
          val py = (abs(rand.nextInt) % (CA.cellsVertical - 2)) + 1
          cellsOnBoundaries += ((px, py))
          (px, py)
        }
        case "Heterogeneous" => {
          val los = rand.nextInt(100) + 1
          // val pair = if (los < nucleationPercentOnBoundaries) {
          val cell = if (los < nucleationPercentOnBoundaries)
            cellsOnBoundaries(rand.nextInt(cellsOnBoundaries.size))
          else
            ((abs(rand.nextInt) % (CA.cellsHorizontal - 2)) + 1,(abs(rand.nextInt) % (CA.cellsVertical - 2)) + 1)
          CA.getNb(cell._1, cell._2).foreach(nb =>
            cellsOnBoundaries += ((nb._1, nb._2)))
          (cell._1, cell._2)
        }
      }
      CA.srxAddArrayCell(coords._1, coords._2, possibleStates(rand.nextInt(Omega)))
      H(coords._1)(coords._2) = 0
    }
  }

  def decreasingNucleation = {
    if (Window.controls.nucleationStart.text.value.toInt - (currentRecStep * Window.controls.nucleationFactor.text.value.toInt) > 0) {
      for (i <- 0 until Window.controls.nucleationStart.text.value.toInt - (currentRecStep * Window.controls.nucleationFactor.text.value.toInt)) {
        val coords = Window.controls.nucleationType.selectionModel().getSelectedItem match {
          case "Homogeneous" => {
            val px = (abs(rand.nextInt) % (CA.cellsHorizontal - 2)) + 1
            val py = (abs(rand.nextInt) % (CA.cellsVertical - 2)) + 1
            cellsOnBoundaries += ((px, py))
            (px, py)
          }
          case "Heterogeneous" => {
            val los = rand.nextInt(100) + 1
            // val pair = if (los < nucleationPercentOnBoundaries) {
            val cell = if (los < nucleationPercentOnBoundaries)
              cellsOnBoundaries(rand.nextInt(cellsOnBoundaries.size))
            else
              ((abs(rand.nextInt) % (CA.cellsHorizontal - 2)) + 1,(abs(rand.nextInt) % (CA.cellsVertical - 2)) + 1)
            CA.getNb(cell._1, cell._2).foreach(nb =>
              cellsOnBoundaries += ((nb._1, nb._2)))
            (cell._1, cell._2)
          }
        }
        CA.srxAddArrayCell(coords._1, coords._2, possibleStates(rand.nextInt(Omega)))
        H(coords._1)(coords._2) = 0
      }
    }
  }

  def heterogenousEnergyDist = {
    H = Array.fill(CA.cellsHorizontal, CA.cellsVertical)(0)
    val currentBoundaries = for {i <- 0 until CA.cellsHorizontal
                                 j <- 0 until CA.cellsVertical
    } {
      val nbs = CA.getNb(i, j)
      if (nbs.exists(nb => CA.caArray(nb._1)(nb._2) != CA.caArray(i)(j))) {
        H(i)(j) = 4
        nbs.foreach(nb =>
          H(nb._1)(nb._2) = 4)
      }
    }
  }
}