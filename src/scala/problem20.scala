object Problem20 {

    def removeAt[A](k: Int, ls: List[A]): (List[A], A) = 
        (ls.take(k) ::: ls.drop(k + 1), ls(k))

}

import Problem20._

println(removeAt(1, List('a, 'b, 'c, 'd)))
