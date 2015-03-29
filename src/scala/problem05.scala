object Problem05 {

    def reverse[A](ls: List[A]): List[A] = ls.reverse

    def reverseRec[A](ls: List[A]): List[A] = ls match {
        case h +: t => reverseRec(t) :+ h
        case Nil => Nil
    }

    def reverseFun[A](ls: List[A]): List[A] =
        ls.foldLeft(List[A]()) {(r, h) => h :: r}
}

import Problem05._

println(reverse(List(1, 1, 2, 3, 5, 8)))
println(reverseRec(List(1, 1, 2, 3, 5, 8)))
