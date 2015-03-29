object Problem09 {

    def pack[A](ls: List[A]): List[List[A]] = ls match {
        case Nil => Nil
        case h :: tail => (h :: tail.takeWhile(_ == h)) :: pack(tail.dropWhile(_ == h))
    }

}

import Problem09._

println(pack(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)))
