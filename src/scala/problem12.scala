object Problem12 {

    def decode[A](ls: List[(Int, A)]): List[A] = ls match {
        case Nil => Nil
        case (c, ch) :: tail => List.fill(c)(ch) ::: decode(tail)
    }

}

import Problem12._

println(decode(List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e))))
