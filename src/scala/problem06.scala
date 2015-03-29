object Problem06 {

    def isPalindrome[A](ls: List[A]): Boolean = ls match {
        case h +: rest :+ t if( h == t) => isPalindrome(rest)
        case h +: rest :+ t => false
        case Nil => true
        case _ => true
    }

    def isPalindrome2[A](ls: List[A]): Boolean = ls == ls.reverse
}

import Problem06._


println(isPalindrome(List(1, 2, 2, 1)))
println(isPalindrome(List(1, 2, 3, 2, 1)))
println(isPalindrome(List(2, 3, 2, 1)))
println(isPalindrome(List(2, 2, 3, 2, 1)))
