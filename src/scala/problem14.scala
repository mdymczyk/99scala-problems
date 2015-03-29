object Problem14 {

    def duplicate[A](ls: List[A]): List[A] = ls match {
        case Nil => Nil
        case h :: tail => h :: h :: duplicate(tail)
    }

}

import Problem14._

println(duplicate(List('a, 'b, 'c, 'c, 'd)))
