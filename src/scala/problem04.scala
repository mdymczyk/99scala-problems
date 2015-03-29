object Problem04 {
    def length[A](ls: List[A]): Int = ls.size

    def length2[A](ls: List[A]): Int = ls match {
        case Nil => 0
        case _ :: t => length2(t) + 1
    }

    def lenTail[A](ls: List[A]): Int = {
        def inner(acc: Int, ls: List[A]): Int = ls match {
            case Nil    => acc
            case _ :: t => inner(acc+1, t)
        }
        inner(0, ls)
    }

    def lenFun[A](ls: List[A]): Int = ls.foldLeft(0) { (c, _) => c + 1 }
}

import Problem04._

println(length(List(1,1,2,3,5,7)))
println(length2(List(1,1,2,3,5,7)))
println(lenTail(List(1,1,2,3,5,7)))
println(lenFun(List(1,1,2,3,5,7)))
