object Problem27 {

    import Problem26._
    def group[A](g: List[Int], ls: List[A]): List[List[A]] = g match {
        case Nil => List(Nil)
        case h :: gt  => combinations(h, ls) flatMap { c =>
            group(gt, ls -- c) map {c :: _}
        }
    }
}

import Problem27._

println(List(2, 2, 5), List("Aldo", "Beat", "Carla", "David", "Evi", "Flip", "Gary", "Hugo", "Ida"))
