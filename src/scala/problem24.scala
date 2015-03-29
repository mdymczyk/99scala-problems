object Problem24 {

    import Problem23._
    def lott(k: Int, max: Int): List[A] = 
        randomSelect(k, List.range(1, max + 1))
}

import Problem24._

println(lotto(6, 49))
