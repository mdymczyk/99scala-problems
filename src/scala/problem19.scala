object Problem19 {

    def rotate[A](k: Int, ls: List[A]): List[A] = {
        def rot(c: Int, curr: List[A], res: List[A]): List[A] = (c, curr) match {
            case (_, Nil) => curr ::: res.reverse
            case (0, _) => curr ::: res.reverse
            case (_, h :: tail) => rot(c - 1, tail, h :: res)
        }

        rot(if (k > 0) k else ls.size + k , ls, Nil)
    }

}

import Problem19._

println(rotate(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)))
println(rotate(-2, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)))
