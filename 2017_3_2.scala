import scala.annotation.tailrec
import scala.io.Source

val ToFind=47

def getDiagonalValue(n: Int, lookup:Int): Int = {
  val result = Math.pow(1 + (2 * (n)), 2)
  if (lookup < result) {
    n
  } else {
    getDiagonalValue((n + 1), lookup)
  }
}

println(s"Result:${getDiagonalValue(0,ToFind)}")
