object Problem22 {

    def range(s: Int, e: Int): List[Int] = e - s match {
        case -1 => Nil
        case x if x > -1 => s :: range(s + 1, e)
        case _ => Nil
    }

}

import Problem22._ 

println(range(4,9))
println(range(4,2))
println(range(4,4))
