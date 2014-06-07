package multiscale

import scala.util.Random
import scala.math._
import scalafx.scene.paint._
import scalafx.scene.canvas.GraphicsContext
import scalafx.application.Platform

object CA {
  val rand = new Random()
  val imgHeight = 768
  val imgWidth = 1024
  var cellSize = 4
  var cellsHorizontal = imgWidth / cellSize
  var cellsVertical = imgHeight / cellSize
  var periodic = true
  var caArray = Array.fill(cellsHorizontal, cellsVertical)(Color.WHITE)
  var srxArray = Array.fill(cellsHorizontal, cellsVertical)(Color.WHITE)
  var inclusions = Set(Color.BLACK)
  var gc: GraphicsContext = null
  def updateArraySize() = {
    cellsHorizontal = imgWidth / cellSize
    cellsVertical = imgHeight / cellSize
    for {
      i <- 0 until cellsHorizontal
      j <- 0 until cellsVertical
    } {
      caArray(i)(j) = Color.WHITE
    }
  }
  def clear() = {
    for {
      i <- 0 until cellsHorizontal
      j <- 0 until cellsVertical
    } {
      caArray(i)(j) = Color.WHITE
      srxArray(i)(j) = Color.WHITE
      MonteCarlo.H(i)(j) = 4
      MonteCarlo.cellsOnBoundaries.clear()
    }
    gc.fill = Color.WHITE
    gc.fillRect(0, 0, imgWidth, imgHeight)
    inclusions = Set(Color.BLACK)
    MonteCarlo.currentRecStep = 0

  }
  def addCell(x: Int, y: Int, col: Color) = {
    caArray(x)(y) = col
    gc.fill = col
    gc.fillRect(x * cellSize, y * cellSize, cellSize, cellSize)
  }

  def addArrayCell(x: Int, y: Int, col: Color) = {
    caArray(x)(y) = col
  }

  def srxAddArrayCell(x: Int, y: Int, col: Color) = {
    srxArray(x)(y) = col
    caArray(x)(y) = col
  }

  def refreshCA = {
    (0 until CA.cellsHorizontal).foreach {
      i =>
        (0 until CA.cellsVertical).foreach {
          j =>
            gc.fill = caArray(i)(j)
            gc.fillRect(i * cellSize, j * cellSize, cellSize, cellSize)        }
    }
  }

  def srxAddCell(x: Int, y: Int, col: Color) = {
    srxArray(x)(y) = col
    caArray(x)(y) = col
    gc.fill = col
    gc.fillRect(x * cellSize, y * cellSize, cellSize, cellSize)
  }

  def addRandomSeed() = {
    var px = 0
    var py = 0
    do {
      px = (abs(rand.nextInt) % (cellsHorizontal - 2)) + 1
      py = (abs(rand.nextInt) % (cellsVertical - 2)) + 1
    } while (caArray(px)(py) != Color.WHITE)
    addCell(px, py, Color(random, random, random, 1))
  }

  def getNb(x: Int, y: Int): List[(Int, Int)] = {
    if (x > 0 && y > 0 && y < cellsVertical - 1 && x < cellsHorizontal - 1) {
      return List((x - 1, y - 1), (x, y - 1), (x + 1, y - 1), (x + 1, y), (x + 1, y + 1), (x, y + 1), (x - 1, y + 1), (x - 1, y))
    } else {
      if (periodic) {
        var xl = x - 1
        var xr = x + 1
        var yu = y - 1
        var yd = y + 1
        if (x == 0) xl = cellsHorizontal - 1
        if (y == 0) yu = cellsVertical - 1
        if (x == cellsHorizontal - 1) xr = 0
        if (y == cellsVertical - 1) yd = 0
        return List((xl, yu), (x, yu), (xr, yu), (xr, y), (xr, yd), (x, yd), (xl, yd), (xl, y))
      } else {
        return List()
      }
    }
  }

  def moore() = {
    val upd = scala.collection.mutable.Map[(Int, Int), Color]()
    for {
      i <- 0 until cellsHorizontal
      j <- 0 until cellsVertical
    } {
      if (caArray(i)(j) != Color.WHITE && !inclusions.exists(c => c == caArray(i)(j))) {
        getNb(i, j).foreach(nb =>
          if (caArray(nb._1)(nb._2) == Color.WHITE) {
            upd += ((nb._1, nb._2) -> caArray(i)(j))
          })
      }
    }

    upd.foreach(e =>
      addCell(e._1._1, e._1._2, e._2))
    upd.clear
  }



  def addIncSq(x: Int, y: Int, r: Int) = {
    for {
      i <- x until x + r
      j <- y until y + r
    } {
      addCell(i, j, Color.BLACK)
    }
  }

  def addIncCi(x: Int, y: Int, r: Int) = {
    val cx: Int = (x + x + r) / 2
    val cy: Int = (y + y + r) / 2
    for {
      i <- x until x + r
      j <- y until y + r
    } {
      if (sqrt(pow(abs(cx - i), 2) + pow(abs(cy - j), 2)) < r / 2) {
        addCell(i, j, Color.BLACK)
      }
    }
  }
}
