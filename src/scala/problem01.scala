object Problem01 {
    def last[A](list: List[A]): A = list.last

    def lastRec[A](list: List[A]): A = list match {
        case h :: Nil => h
        case _ :: t => lastRec(t)
        case Nil => throw new NoSuchElementException
    }
}

import Problem01._

println(last(List(1,2,3,4,5,6,7,8)))
println(lastRec(List(1,2,3,4,5,6,7,8)))
