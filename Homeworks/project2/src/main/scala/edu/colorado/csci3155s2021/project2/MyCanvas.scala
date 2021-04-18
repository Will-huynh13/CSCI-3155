package edu.colorado.csci3155s2021.project2

/* A class to maintain a canvas */
import java.awt.geom.{Ellipse2D, Rectangle2D}
import java.awt.{Graphics2D}

/* A figure is a sealed trait. It can be a Polygon or a "MyCircle"*/
sealed trait Figure {
    def getBoundingBox: (Double, Double, Double, Double)
    def translate(x: Double, y: Double): Figure
    def render(g: Graphics2D, scaleX: Double, scaleY: Double, shiftX: Double, shiftY: Double): Unit
}

/*
 Class Polygon
   A polygon is defined by a list of its vertices
 */
case class Polygon(val cList: List[(Double, Double)]) extends Figure {
    //TODO: Define the bounding box of the polygon
    // This function returns a 4-tuple (xmin, xmax, ymin, ymax)
    override def getBoundingBox: (Double, Double, Double, Double ) = {
        //using scala maxBy and minBy to find the min and max in a tuple to make things easier
        // essentially (0,2).maxBy would return 2
        // we pass in the tuples and sort accordingly
        val newYMin = cList.minBy(_._2)._2
        val newXMin = cList.minBy(_._1)._1
        val newYMax = cList.maxBy(_._2)._2
        val newXMax = cList.maxBy(_._1)._1
        (newXMin,newXMax,newYMin,newYMax)
    }

    //TODO: Create a new polygon by shifting each vertex in cList by (x,y)
    //    Do not change the order in which the vertices appear
    override def translate(x: Double, y: Double): Polygon = {
        //returns a new polygon
        new Polygon(cList.map((element: (Double,Double)) => (element._1 + x, element._2 + y))) // used map to add x and y
    }

    // Function: render -- draw the polygon. Do not edit this function.
    override def render(g: Graphics2D, scaleX: Double, scaleY: Double, shiftX: Double, shiftY: Double) = {
        val xPoints: Array[Int] = new Array[Int](cList.length)
        val yPoints: Array[Int] = new Array[Int](cList.length)
        for (i <- 0 until cList.length){
            xPoints(i) = ((cList(i)._1 + shiftX )* scaleX).toInt
            yPoints(i) = ((cList(i)._2 + shiftY) * scaleY).toInt
        }
        g.drawPolygon(xPoints, yPoints, cList.length)

    }
}

/*
  Class MyCircle
  Define a circle with a given center c and radius r
 */
case class MyCircle(val c: (Double, Double), val r: Double) extends Figure {
    //TODO: Define the bounding box for the circle
    override def getBoundingBox: (Double, Double, Double, Double) = {
        val newXMin = c._1 - r
        val newXMax = c._1 + r
        val newYMin = c._2 - r
        val newYMax = c._2 + r
        (newXMin,newXMax,newYMin,newYMax)
    }


    //TODO: Create a new circle by shifting the center
    override def translate(x: Double, y: Double): MyCircle = {
        new MyCircle((c._1 + x, c._2 + y),r)
    }


    // Function: render -- draw the circle. Do not edit this function
    override def render(g: Graphics2D, scaleX: Double, scaleY: Double, shiftX: Double, shiftY: Double) = {
        val centerX = ((c._1 + shiftX) * scaleX) .toInt
        val centerY = ((c._2 + shiftY) * scaleY) .toInt
        val radX = (r * scaleX).toInt
        val radY = (r * math.abs(scaleY)).toInt
        //g.draw(new Ellipse2D.Double(centerX, centerY, radX, radY))
        g.drawOval(centerX-radX, centerY-radY, 2*radX, 2*radY)
    }
}

/*
  Class : MyCanvas
  Define a canvas through a list of figure objects. Figure objects can be circles or polygons.
 */

class MyCanvas (val listOfObjects: List[Figure]) {
    // TODO: Write a function to get the boundingbox for the entire canvas.
    // Hint: use existing boundingbox functions defined in each figure.
    def getBoundingBox: (Double, Double, Double, Double) = {
        // we first map so the map returns tuples, from there we use the maxBy and maxMin accordingly to extract
        // similar to the logic with polygon
        //we used the .getBoundingBox to obtain the tuples for each figure in the list of figures
        val newYMax = listOfObjects.map((fig: Figure) => fig.getBoundingBox).maxBy(_._4)._4
        val newYMin = listOfObjects.map((fig: Figure) => fig.getBoundingBox).minBy(_._3)._3
        val newXMin = listOfObjects.map((fig: Figure) => fig.getBoundingBox).minBy(_._1)._1
        val newXMax = listOfObjects.map((fig: Figure) => fig.getBoundingBox).maxBy(_._2)._2
        (newXMin,newXMax,newYMin,newYMax) // returning list of 4-tuple
    }
    //TODO: Write a function to translate each figure in the canvas by shiftX, shiftY
    def translate(shiftX: Double, shiftY: Double): MyCanvas = {
        new MyCanvas(listOfObjects.map(fig => fig.translate(shiftX,shiftY)))
    }


    //TODO: Write a function that will return a new MyCanvas object that places
    // all the objects in myc2 to the right of the objects in this MyCanvas.
    // refer to the notebook documentation on how to perform this.
    def placeRight(myc2: MyCanvas):MyCanvas = {
    //        Let  (𝑥min,1,𝑥max,1,𝑦min,1,𝑦max,1)  be the result of  𝚋𝚘𝚞𝚗𝚍𝚒𝚗𝚐𝙱𝚘𝚡(𝑐1) .
    //        Let  (𝑥min,2,𝑥max,2,𝑦min,2,𝑦max,2)  be the result of  𝚋𝚘𝚞𝚗𝚍𝚒𝚗𝚐𝙱𝚘𝚡(𝑐2) .
    //        Define  𝑥𝑆ℎ𝑖𝑓𝑡=(𝑥max,1−𝑥min,2) .
    //        Define  𝑦𝑆ℎ𝑖𝑓𝑡=(𝑦max,1−𝑦min,1)/2−(𝑦max,2−𝑦min,2)/2 .
    //          Let  𝑐̂ 2  be the canvas obtained by  𝚝𝚛𝚊𝚗𝚜𝚊𝚝𝚎(𝑐2,𝑥𝑆ℎ𝑖𝑓𝑡,𝑦𝑆ℎ𝑖𝑓𝑡)
    //        Define the result of  𝚙𝚕𝚊𝚌𝚎𝚁𝚒𝚐𝚑𝚝(𝑐1,𝑐2)=𝚘𝚟𝚎𝚛𝚕𝚊𝚙(𝑐1,𝑐̂ 2) .
        val canvas1 = this.getBoundingBox
        val canvas2 = myc2.getBoundingBox
        val xShift = canvas1._2 - canvas2._1
        val yShift = ((canvas1._4 - canvas1._3) / 2) - ((canvas2._4 - canvas2._3) / 2)
        val canvas3 = myc2.translate(xShift,yShift)
        this.overlap(canvas3)
    }

    //TODO: Write a function that will return a new MyCanvas object that places
    // all the figures in myc2 on top of the figures in this MyCanvas.
    // refer to the notebook documentation on how to perform this.
    def placeTop(myc2: MyCanvas): MyCanvas = {
    //        Let  (𝑥min,1,𝑥max,1,𝑦min,1,𝑦max,1)  be the result of  𝚋𝚘𝚞𝚗𝚍𝚒𝚗𝚐𝙱𝚘𝚡(𝑐1) .
    //        Let  (𝑥min,2,𝑥max,2,𝑦min,2,𝑦max,2)  be the result of  𝚋𝚘𝚞𝚗𝚍𝚒𝚗𝚐𝙱𝚘𝚡(𝑐2) .
    //        Define  𝑥𝑆ℎ𝑖𝑓𝑡=(𝑥max,1−𝑥min,1)/2−(𝑥max,2−𝑥min,2)/2 .
    //          Define  𝑦𝑆ℎ𝑖𝑓𝑡=(𝑦max,1−𝑦min,2) .
    //        Let  𝑐̂ 2  be the canvas obtained by  𝚝𝚛𝚊𝚗𝚜𝚊𝚝𝚎(𝑐2,𝑥𝑆ℎ𝑖𝑓𝑡,𝑦𝑆ℎ𝑖𝑓𝑡)
    //        Define the result of  𝚙𝚕𝚊𝚌𝚎𝚃𝚘𝚙(𝑐1,𝑐2)=𝚘𝚟𝚎𝚛𝚕𝚊𝚙(𝑐1,𝑐̂ 2).
        val canvas1 = this.getBoundingBox
        val canvas2 = myc2.getBoundingBox
        val xShift = ((canvas1._2 - canvas1._1) / 2) - ((canvas2._2 - canvas2._1) / 2)
        val yShift = canvas1._4 - canvas2._3
        val canvas3 = myc2.translate(xShift,yShift)
        this.overlap(canvas3)
    }

    //TODO: Write a function that will rotate each figure about the center of its bounding box in the canvas using
    // the angle `ang` defined in radians.
    // The writeup provided describes how to implement the rotation.
    // Hint: Write helper functions to rotate a Polygon and a circle. Then you can simply use
    // translation, followed by rotation of individual objects and translation back.
    def rotate(angRad: Double): MyCanvas = {
        //        Given a Canvas object  𝑐  , its rotation by  𝜃  in radians is denoted  𝚛𝚘𝚝𝚊𝚝𝚎(𝑐,𝜃)  and yields a new canvas.
        //        Recall from your calculus class that rotating a point  (𝑥,𝑦)  by angle  𝜃  in radians around the origin yields
        //        𝑥′=𝑥cos(𝜃)−𝑦sin(𝜃),   𝑦′=𝑥sin(𝜃)+𝑦cos(𝜃)

        val xC = ((this.getBoundingBox._1 + this.getBoundingBox._2 ) / 2)// xC value
        val yC = ((this.getBoundingBox._3 + this.getBoundingBox._4 ) / 2)// yC value
        val t = this.translate(-1*xC,-1*yC) // translate by -xc and -yc
       val listObj = t.listOfObjects.map{
            case circle: MyCircle => {
                new MyCircle((circle.c._1*math.cos(angRad) - circle.c._2*math.sin(angRad), circle.c._1*math.sin(angRad) + circle.c._2*math.cos(angRad)),circle.r)
            }
            case polygonal: Polygon => {
                new Polygon(polygonal.cList.map((pair: (Double,Double)) => (pair._1*math.cos(angRad) - pair._2*math.sin(angRad),pair._1*math.sin(angRad) + pair._2*math.cos(angRad))))
            }
        }
        new MyCanvas(listObj).translate(xC,yC)
    }


    def overlap(c2: MyCanvas): MyCanvas = {
        new MyCanvas(listOfObjects ++ c2.listOfObjects)
    }

    // Function to draw the canvas. Do not edit.
    def render(g: Graphics2D, xMax: Double, yMax: Double) = {
        val (lx1, ux1, ly1, uy1) = this.getBoundingBox
        val shiftx = -lx1
        val shifty = -uy1
        val scaleX = xMax/(ux1 - lx1  + 1.0)
        val scaleY = yMax/(uy1 - ly1 + 1.0)
        listOfObjects.foreach(f => f.render(g,scaleX, -scaleY, shiftx, shifty))
    }

    // DO NOT EDIT THE CODE BELOW
    override def toString: String = {
        listOfObjects.foldLeft[String] ("") { case (acc, fig) => acc ++ fig.toString }
    }

    // DO NOT EDIT
    def getListOfObjects: List[Figure] = listOfObjects

    // DO NOT EDIT
    def numPolygons: Int =
        listOfObjects.count {
            case Polygon(_) => true
            case _ => false }

    // DO NOT EDIT
    def numCircles: Int = {
        listOfObjects.count {
            case MyCircle(_,_) => true
            case _ => false }
    }
    // DO NOT EDIT
    def numVerticesTotal: Int = {
        listOfObjects.foldLeft[Int](0) ((acc, f) =>
            f match {
                case Polygon(lst1) => acc + lst1.length
                case _ => acc
            }
        )
    }
}
