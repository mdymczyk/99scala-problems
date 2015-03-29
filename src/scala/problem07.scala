object Problem07 {

    def flatten(ls: List[Any]): List[Any] = ls flatMap {
            case l: List[_] => flatten(l)
            case x => List(x)
    }

}

import Problem07._

println(flatten(List(List(1, 1), 2, List(3, List(3, List(5, 8))))))
