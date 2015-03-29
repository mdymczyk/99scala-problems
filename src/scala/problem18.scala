object Project18 {

    def slice[A](s: Int, e: Int, ls: List[A]): List[A] = {
        def slice(cs: Int, n: Int, curr: List[A]): List[A] = (cs, curr) match {
            case (_, Nil) => Nil
            case (0, _) => curr.take(n)
            case (_, h :: tail) => slice(cs - 1, n, tail)
        }
        slice(s, e - s, ls)
    }

    def sliceBuiltin[A](s: Int, e: Int, ls: List[A]): List[A] = 
        ls.slice(s, e)

}

import Project18._

println(slice(3, 7, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)))
println(sliceBuiltin(3, 7, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)))
