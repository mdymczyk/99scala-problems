object Problem10 {

    import Problem09._
    def encode[A](ls: List[A]): List[(Int, A)] = ls match {
        pack(ls) map { t => (t.length, t.head) }
    }
}

import Problem10._

println(encode(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)))
