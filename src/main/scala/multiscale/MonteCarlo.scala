package multiscale

import scalafx.scene.paint.Color
import scalafx.application.Platform
import scala.math._
import scala.util.Random
import scala.util.Failure
import scala.collection.mutable.Map

object MonteCarlo {
  val ca = CA.caArray
  val rand = new Random()
  val Omega = 20
  val MCS = 5
  val steps = CA.cellsHorizontal * CA.cellsVertical
  val Jgb = 1
  var H = Array.fill(CA.cellsHorizontal, CA.cellsVertical)(rand.nextInt(5))
  val possibleStates = {
    for (i <- 0 until Omega) yield {
      Color(random, random, random, 1)
    }

  }

  def randomize() = {

    val randomized = scala.collection.mutable.Map[(Int, Int), Color]()

    (0 until CA.cellsHorizontal).foreach {
      i =>
        (0 until CA.cellsVertical).foreach {
          j =>
            CA.addCell(i, j, possibleStates(rand.nextInt(Omega)))
        }
    }

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
    CA.gc.fillRect(0, 0, 800, 600)

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
    (0 until steps).foreach {
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
            // Platform.runLater {
            CA.addCell(x, y, nextOrientation)
            //  }
          }
        }
    }
  }

  def runRecrystalization = {

    (0 until steps).foreach {
      s =>
      // 1. choose random cell
        val x = rand.nextInt(CA.cellsHorizontal)
        val y = rand.nextInt(CA.cellsVertical)
        val Jgb_recr = 0.5

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
          val Egb_afterChange = Jgb_recr * sum
          // 5. Compare and switch

          if (Egb_afterChange <= Egb) {
            H(x)(y) = 0
            // recrystalizedCells += ((x, y))
            CA.srxAddCell(x, y, nextOrientation)
          }
        }
    }
    //     }
    //recrystalizedCells
  }

  def runSRX = {
    (0 until 100).foreach {
      s =>
      // 1. choose random recrystalized cell
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
            // Platform.runLater {
            CA.addCell(x, y, nextOrientation)
            //  }
          }
        }
    }
  }


  def constantNucleation = {
    for (i <- 0 until 100) {
      val px = (abs(rand.nextInt) % (CA.cellsHorizontal - 2)) + 1
      val py = (abs(rand.nextInt) % (CA.cellsVertical - 2)) + 1
      CA.srxAddCell(px, py, possibleStates(rand.nextInt(Omega)))
      H(px)(py) = 0
    }
  }
}