import scala.annotation.tailrec
import scala.io.Source

val filename = "input_2017_2_1.txt"
//val filename = "test.txt"

//5 9 2 8
//9 4 7 3
//3 8 6 5

def MinMax (input:List[Int],comparator:Int, fn: (Int, Int) => Boolean) : Int ={
  input match {
    case Nil => comparator
    case x::xt => {if(fn(x,comparator)){MinMax(xt,x,fn)} else {MinMax(xt,comparator,fn)}}
  }
}

def getMax(input:List[Int]): Int = {
  val > = (a:Int,b:Int) => a > b
  MinMax(input,0,>)
}

def isDivisible (input:List[Int],acc : Int) : Int ={
  input match {
    case Nil => acc
    case x::xt => {
      val returnVal=xt map (res => {
        if((x % res)==0){
          x/res
        }
        else if((res % x)==0){
          res/x
        }
        else{
          acc
        }
      })
      if(getMax(returnVal)==0){
        isDivisible(xt,acc)
      }else{
        isDivisible(xt,getMax(returnVal))
      }


    }
  }
}



def lineCheckSum(line:String): Int = {
  val lineList = line.split(" ").toList.map(x => x.toInt)
  println(s"BLA:${isDivisible(lineList,0)}")
  isDivisible(lineList,0)
}
println((Source.fromFile(filename).getLines.toList map (line => lineCheckSum(line))).fold(0)(_+_))




