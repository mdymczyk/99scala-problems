object Problem26 {

    def combinations[A](k: Int, ls: List[A]): List[List[A]] = (k, ls) match {
        case (0, _) => List(List())
        case (_, Nil) => Nil
        case (_, h :: tail) => combinations(k, tail) ::: combinations(k - 1, tail).map(h :: _) 
    }

    // Same concept, different style
    def flatMapSublists[A, B](ls: List[A])(f: (List[A]) => List[B]): List[B] = ls match {
        case Nil => List(Nil)
        case sublist@(h :: tail) => f(sublist) ::: flatMapSublists(tail)(f)
    }

    def combinations2[A](k: Int, ls: List[A]): List[List[A]] = {
        if(k == 0) List(Nil)
        else flatMapSublists(ls) { sl =>
            combinations2(k - 1, ls.tail) map (sl.head :: _)
        }
    }
}

import Problem26._

//println(combinations(3, List('a, 'b, 'c, 'd, 'e, 'f)))
println(combinations(2, List('a, 'b, 'c, 'd)))
