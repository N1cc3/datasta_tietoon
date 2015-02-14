import scala.collection.mutable.ArrayBuffer
import scala.io.Source

object pagerank extends App {

  var L = ArrayBuffer[ArrayBuffer[Int]]() // Matrix of links between pages
  var e = 0.001 // epsilon default value

  // Load file
  if (args.length <= 1) {
    println("Usage example: <program> file epsilon")
  } else {
    if (args.length >= 2) {
      e = args(1).toDouble
    }
    var lineNum: Int = 0
    Source.fromFile(args.apply(0)).getLines.foreach { line =>
      L += ArrayBuffer[Int]()
      line.split(" ").foreach { c =>
        L(lineNum) += c.toInt
      }
      lineNum += 1
    }
  }

  var n = L(1).length
  var pr: Array[Double] = Array.fill(n)(0)

  // Calculate PageRanks
  for (i <- 0 until L.length) {
    for (j <- 0 until n) {
      if (L(i)(j) == 1) {
        pr(j) += (1.0 / L.length - e) / L(i).sum + e / n
      } else if (L(i).sum == 0) {
        pr(j) += 1.0 / n
      } else {
        pr(j) += e / n
      }
    }
  }

  // Print results
  pr.foreach(cell => println(cell))

}
