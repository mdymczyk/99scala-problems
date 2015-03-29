object Problem21 {

    def insertAt[A](el: A, k: Int, ls: List[A]): List[A] =
        ls.take(k) ::: el :: ls.drop(k)

    def insertAt2[A](el: A, k: Int, ls: List[A]): List[A] = ls.splitAt(k) match {
        case (pre, post) => pre ::: el :: post
    }
}

import Problem21._

println(insertAt('new, 1, List('a, 'b, 'c, 'd)))
