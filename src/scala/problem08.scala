object Problem08 {

    def compressRecursive[A](ls: List[A]): List[A] = ls match {
        case Nil => Nil
        case h :: tail => h :: compressRecursive(tail.dropWhile(_ == h))
    }

    def compressTailRecursive[A](ls: List[A]): List[A] = {
        def compTail(res: List[A], curr: List[A]): List[A] = curr match {
            case h :: tail => compTail(h :: res, tail.dropWhile(_ == h))
            case Nil => res.reverse
        }
        compTail(Nil, ls)
    }

    def compressFunctional[A](ls: List[A]): List[A] = {
        ls.foldRight(List[A]()) { (h,r) =>
            if(r.isEmpty || r.head != h) h :: r
            else r
        }
    }

}

import Problem08._

println(compressRecursive(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)))
println(compressTailRecursive(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)))
println(compressFunctional(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)))
