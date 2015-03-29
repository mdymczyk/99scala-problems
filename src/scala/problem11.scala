object Problem11 {

    import Problem10._
    def encodeMod[A](ls: List[A]): List[Either[A, (Int, A)]] = 
        encode(ls) map { t => if(t._1 == 1) Left(t._2) else Right(t) }

}

import Problem11._

println(encodeMod(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)))
