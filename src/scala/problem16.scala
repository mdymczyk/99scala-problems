object Problem16 {

    def drop[A](n: Int, ls: List[A]): List[A] = ls match {
        case Nil => Nil
        case list => list.take(n - 1) ::: drop(n, ls.drop(n))
    }

    def dropRec[A](n: Int, ls: List[A]): List[A] = {
        def dropR[A](c: Int, cur: List[A]): List[A] = (c, cur) match {
            case (_, Nil) => Nil
            case (1, _ :: tail) => dropR(n, tail)
            case (_, h :: tail) => h :: dropR(c - 1, tail)
        }
        dropR(n, ls)
    }

    def dropRecTail[A](n: Int, ls: List[A]): List[A] = {
        def drop[A](n: Int, curr: List[A], res: List[A]): List[A] = (c, curr) match {
            case (_, Nil) => res.reverse
            case (1, _ :: tail) => drop(n, tail, res)
            case (_, h :: tail) => drop(c - 1, tail, h :: res)
        }
        drop(n, ls, Nil)
    }

    def dropFunctional[A](n: Int, ls: List[A]): List[A] = 
        ls.zipWithIndex filter{ v => (v._2 + 1) % n != 0 } map { _._1 }

}

import Problem16._

println(drop(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)))
