object Problem10 {

    def encode[A](ls: List[A]): List[(Int, A)] = ls match {
        case Nil => Nil
        case h :: tail => (tail.takeWhile(_ == h).size + 1, h) :: encode(tail.dropWhile(_ == h))
    }
}

import Problem10._

println(encode(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)))
