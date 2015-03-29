object Problem02 {

    def penultimate[A](list: List[A]): A = list match {
        case h :: _ :: Nil => h
        case _ :: t => penultimate(t)
        case Nil => throw new NoSuchElementException
    }

    def penultimateBuiltin[A](list: List[A]): A = 
        if(list.isEmpty) throw new NoSuchElementException
        else list.init.last

    def lastNthRecursive[A](n: Int, ls: List[A]): A = {
        def lastNthR(count: Int, resultList: List[A], curList: List[A]): A =
            curList match {
                case Nil if count > 0 => throw new NoSuchElementException
                case Nil              => resultList.head
                case _ :: tail        =>
                    lastNthR(count - 1,
                            if (count > 0) resultList else resultList.tail,
                            tail)
            }
        if (n <= 0) throw new IllegalArgumentException
        else lastNthR(n, ls, ls)
    }
}

import Problem02._

println(penultimate(List(1, 1, 2, 3, 5, 8)))
println(penultimateBuiltin(List(1, 1, 2, 3, 5, 8)))
