object Problem28 {

    def lsort[A](ls: List[List[A]]): List[List[A]] = 
        ls.sortBy(l => l.length)

    def lsort2[A](ls: List[List[A]]); List[List[A]] =
        ls.sort { _.length < _.length )

    import Problem10.encode

    def lsortFreq[A](ls: List[List[A]]): List[List[A]] = {
        val freqs = Map(encode(ls map { _.length } sort { _ < _ }) map { _.swap }:_*)
        ls sort { (e1, e2) => freqs(e1.length) < freqs(e2.length) }
    }
}

import Problem28._

println(lsort(List(List('a, 'b, 'c), List('d, 'e), List('f, 'g, 'h), List('d, 'e), List('i, 'j, 'k, 'l), List('m, 'n), List('o))))


