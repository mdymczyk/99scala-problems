object Problem03 {

    def nth[A](n: Int, ls: List[A]): A = 
        if(ls.size < n) throw new IllegalArgumentException
        else ls.take(n).last

    def nth2[A](n: Int, ls: List[A]): A = 
        if(ls.size < n) throw new IllegalArgumentException
        else ls(n)

    def nthNotBuiltin(n: Int, ls: List[A]): A = (n, ls) match {
        case (0, h :: _) => h
        case (n, _ :: t) => nthNotBuiltin(n - 1, t)
        case (_, Nil)    => throw new NoSuchElementException

}

import Problem03._

println(nth(2, List(1, 1, 2, 3, 5, 8)))
println(nth(3, List(1, 1, 2, 3, 5, 8)))
