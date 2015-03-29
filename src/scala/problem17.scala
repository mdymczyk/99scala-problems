object Project17 {

    def split[A](n: Int, ls: List[A]): List[List[A]] = {
        ls.take(n) :: List(ls.drop(n))
    }

    def splitRec[A](n: Int, ls: List[A]): List[List[A]] = {
        def split(c: Int, curr: List[A], res: List[A]): List[List[A]] = (c, curr) match {
            case (_, Nil) => List(res.reverse, Nil)
            case (0, _) => List(res.reverse, curr)
            case (_, h :: tail) => split(c - 1, tail, h :: res)
        }
        split(n, ls, Nil)
    }

}

import Project17._

println(split(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)))
println(splitRec(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)))
