package dmitry

import java.awt.{Canvas, Color}
import javax.swing.{JFrame, SwingUtilities}

/**
  * Represents an ant.
  * @author Dmitry Dobrynin <dobrynya@inbox.ru>
  *         Created at 21.03.2016 22:50
  */
trait Ant {
  var x = width / 2
  var y = height / 2
  var dir = (1, 0)

  def rotations: Map[(Int, Int), ((Int, Int), (Int, Int))] = Map(
    (1, 0) -> ((0, -1), (0, 1)),
    (0, 1) -> ((1, 0), (-1, 0)),
    (-1, 0) -> ((0, 1), (0, -1)),
    (0, -1) -> ((-1, 0), (1, 0))
  )
  var filled = Set.empty[(Int, Int)]

  def color: Boolean = filled contains (x, y) // true - black, false - white

  def invertColor(): Boolean = {
    val current = color
    setColor(x, y, !current)
    if (color) filled -= ((x, y)) else filled += ((x, y))
    current
  }

  def width: Int
  def height: Int

  /**
    * Provides ability to change color of a pixel on canvas.
    * @param x specifies x coordinate
    * @param y specifies y coordinate
    * @param color specifies whether to enlighten a pixel
    */
  def setColor(x: Int, y: Int, color: Boolean): Unit

  /**
    * Changes current direction.
    * @param color specifies color of current location
    */
  private def rotate(color: Boolean): Unit = {
    dir = rotations(dir) match {
      case (left, right) => if (color) right else left
    }
  }

  /**
    * Makes a next move.
    */
  def move(): Unit = {
    rotate(invertColor())
    x += dir._1
    if (x < 0) x = width - 1 else if (x == width) x = 0
    y += dir._2
    if (y < 0) y = height - 1 else if (y == height) y = 0
  }
}

object testingAnt extends App {
  val frmMain = new JFrame()
  frmMain.setTitle("Langton's Ant")
  frmMain.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
  frmMain.setSize(400, 400)
  val cnvs = new Canvas()
  cnvs.setSize(400, 400) // double-sized the canvas as we have double-sized pixels
  frmMain.add(cnvs)
  frmMain.setVisible(true)
  val g = cnvs.getGraphics
  g.setColor(new Color(150, 0, 255))

  val ant = new Ant {
    def width = 200
    def height = 200
    def setColor(x: Int, y: Int, color: Boolean) =
      SwingUtilities.invokeLater(new Runnable {
        def run() =
          if (color) g.fillRect(x << 1, y << 1, 2, 2)
          else g.clearRect(x << 1, y << 1, 2, 2)
      })
  }

  while (!Thread.currentThread().isInterrupted) {
    ant.move()
    Thread.sleep(10)
  }
}
