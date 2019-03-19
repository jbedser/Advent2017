import scala.annotation.tailrec
import scala.io.Source

val filename = "input_2017_2_1.txt"

def MinMax (input:List[Int],comparator:Int, fn: (Int, Int) => Boolean) : Int ={
  input match {
    case Nil => comparator
    case x::xt => {if(fn(x,comparator)){MinMax(xt,x,fn)} else {MinMax(xt,comparator,fn)}}
  }
}

def lineCheckSum(line:String): Int = {
  val lineList=line.split(" ").toList.map(x => x.toInt)
  val > = (a:Int,b:Int) => a > b
  val < = (a:Int,b:Int) => a < b
  val min:Int=MinMax(lineList,Int.MaxValue,<)
  var max:Int=MinMax(lineList,0,>)

  Math.abs(max-min)
}

println((Source.fromFile(filename).getLines.toList map (line => lineCheckSum(line))).foldLeft(0)(_ + _))




