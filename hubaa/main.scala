import scala.collection.mutable.ArrayBuffer
import scala.io.Source

object Main extends App {
	private var hubs = ArrayBuffer[Node]()	// pizza
	private var auths = ArrayBuffer[Node]()	// topping

	// Load file and set up network
	if (args.length <= 0) {
		println("Usage example: <program> file iterations")
	} else {
		var auths_count = 0
		Source.fromFile(args.apply(0)).getLines.foreach{ line =>
			hubs += new Node()
			var idx = 0
			line.split(" ").foreach{ c =>
				idx+=1;
				if (idx > auths_count) {
					auths_count+=1;
					auths += new Node()
				}
				if (c == "1") {
					hubs.last.addLink(auths.apply(idx-1))
					auths.apply(idx-1).addLinker(hubs.last)
				}
			}
		}
		// Network init
		var nSqrtInv = 1 / math.sqrt(auths.size + hubs.size)
		loop((node: Node) => node.setVars(nSqrtInv))

		// Main iteration
		for (i <- 1 to args.apply(1).toInt) {
			var authSum = 0.0
			var hubSum = 0.0
			var sums = (0.0, 0.0)
			loop((node: Node) => {
				sums = node.update()
				authSum += math.pow(sums._1, 2)
				hubSum += math.pow(sums._2, 2)
			})
			loop((node: Node) => { node.scale(math.sqrt(authSum), math.sqrt(hubSum)) })
		}		
		
		// Print result
		// loop((node: Node) => println(node.auth + "\t" + node.hub))
		loop((node: Node) => println(format(node.auth) + "\t" + format(node.hub)))
	}

	private def loop(callback: (Node) => Unit) = {
		for (node <- hubs) callback(node)
		for (node <- auths) callback(node)
	}
	
	private def format(d: Double): String = {
		"%2.4f".format(d).reverse.padTo(6, " ").reverse.mkString
	}

}

class Node() {
	var auth = 0.0
	var hub = 0.0
	var authNew = 0.0
	var hubNew = 0.0
	var linkers = ArrayBuffer[Node]()
	var links = ArrayBuffer[Node]()

    def setVars(nSqrtInv: Double) = {
		auth = nSqrtInv;
		hub = nSqrtInv;
	}
	
	def addLinker(node: Node) = linkers += node
	def addLink(node: Node) = links += node
	
	def update() = {
		authNew = 0
		hubNew = 0
		for (node <- linkers)
			authNew += node.hub
		for (node <- links)
			hubNew += node.auth
		(authNew, hubNew)
	}

	def scale(Ascale: Double, Hscale: Double) = {
		auth = authNew / Ascale
		hub = hubNew / Hscale
	}
}
