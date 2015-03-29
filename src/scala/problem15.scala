object Problem15 {

    def duplicateN[A](n: Int, ls: List[A]): List[A] = ls match {
        case Nil => Nil
        case h :: tail => List.fill(n)(h) ::: duplicateN(n, tail)
    }

}

import Problem15._

println(duplicateN(3, List('a, 'b, 'c, 'c, 'd)))
