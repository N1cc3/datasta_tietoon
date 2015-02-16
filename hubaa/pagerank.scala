import scala.collection.mutable.ArrayBuffer
import scala.io.Source

object pagerank extends App {

  var L = ArrayBuffer[ArrayBuffer[Int]]() // Matrix of links between pages
  var e = 0.001 // epsilon default value

  // Load file
  if (args.length < 1) {
    println("Usage example: <program> file iterations [epsilon]")
  } else {
    var iterations = args(1).toInt
    if (args.length >= 3) {
      e = args(2).toDouble
    }
    var lineNum: Int = 0
    Source.fromFile(args.apply(0)).getLines.foreach { line =>
      L += ArrayBuffer[Int]()
      line.split(" ").foreach { c =>
        L(lineNum) += c.toInt
      }
      lineNum += 1
    }

    var n = L.length // Number of pages
    var rel = 1.0 / n // Initial amount of relevancy per page
    var pr: Array[Array[Double]] = Array.fill(iterations + 1, n)(0.0)
    pr(0) = Array.fill(n)(rel)
    var in = 0.0 // Incoming pagerank

    // Calculate PageRank
    for (iter <- 0 until iterations) {
      for (i <- 0 until n) {
        pr(iter+1)(i) = e
        in = 0.0
        for (j <- 0 until n) {
          if (L(i)(j)==1) {
            in += pr(iter)(j) / L.transpose.apply(j).sum
          } else if (L.transpose.apply(j).sum == 0) {
            in += pr(iter)(j) / (n-1)
          }
        }
        pr(iter+1)(i) += (1.0-e)*in
      }
    println("Iteration " + (iter + 1))
    pr(iter + 1).foreach(cell => println(cell))
    }

    // Normalize vector
    var sum = pr(pr.length-1).sum
    for (k <- 0 until n) pr(pr.length-1)(k) = pr(pr.length-1)(k) / sum

    // Print results
    println("Result normalized:")
    pr.last.foreach(cell => println(cell))

  }

}
