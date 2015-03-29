object Problem23 {

    import scala.util._

    def randomSelect[A](k: Int, ls: List[A]): List[A] = k match {
        case 0 => Nil
        case _ => {
            val (rem, sel): (List[A], A) = removeK(Random.nextInt(ls.size), ls)
            sel :: randomSelect(k - 1, rem)
        }
    }
    
    def removeK[A](n: Int, l: List[A]): (List[A], A) = {
        (l.take(n) ::: l.drop(n + 1), l(n))
    }

}

import Problem23._

println(randomSelect(3, List('a, 'b, 'c, 'd, 'f, 'g, 'h)))
