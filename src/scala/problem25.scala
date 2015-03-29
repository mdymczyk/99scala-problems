object Problem25 {

    import scala.util._
    import scala.reflect._

    def randomPermute[A](ls: List[A]): List[A] = ls match {
        case Nil => Nil
        case _ => {
            val n = Random.nextInt(ls.size)
            ls(n) :: randomPermute(ls.take(n) ::: ls.drop(n + 1))
        }
    }

    def fisherYates[A : ClassTag](ls: List[A]): List[A] = {
        val rnd = new Random
        val a = ls.toArray
        for (i <- a.length - 1 to 1 by -1) {
            val r = rnd.nextInt(i + 1)
            val temp = a(i)
            a.update(i, a(r))
            a.update(r, temp)
        }
        a.toList
    }

}

import Problem25._

println(randomPermute(List('a, 'b, 'c, 'd, 'e, 'f)))
println(fisherYates(List('a, 'b, 'c, 'd, 'e, 'f)))
