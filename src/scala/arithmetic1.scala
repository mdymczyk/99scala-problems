package arithmetic {

    class S99Int(val start: Int) {
        import S99Int._

        def isPrime: Boolean = 
            (start > 1) && (primes takeWhile { _ <= Math.sqrt(start) } forall { start % _ != 0 }) 
        
        def isPrime2: Boolean =
            sieve contains start
   
        // Problem33
        def isCoprimeTo(o: Int): Boolean =
            gcd(start, o) == 1

        // Problem34
        def totient: Integer = 
            (1 to start) filter { _.isCoprimeTo(start) } size
    
        // Problem35
        def primeFactors: List[Int] = {
            def factors(n: Int, ps: Stream[Int]): List[Int] =
                if (n.isPrime) List(n)
                else if (n % ps.head == 0) ps.head :: factors(n / ps.head, ps)
                else factors(n, ps.tail)
            factors(start, primes)
        }

        def primeFactorsMult: List[(Int, Int)] =
            start.primeFactors.groupBy(identity).values.toList.map( l => (l.head, l.size) )
    }

    object Functional1 {
        class PipedObject[T] private[Functional1] (value: T) {
            def |>[R] (f : T => R) = f(this.value)
        }

        import scala.language.implicitConversions

        implicit def toPiped[T] (value: T) = new PipedObject[T](value)
    }

    object S99Int {
        import Functional1._
        import scala.language.implicitConversions

        implicit def int2S99Int(i: Int): S99Int = new S99Int(i)

        // Problem31
        val LIMIT = 1000
        val sieve =  (2 to LIMIT) |> (r => r.foldLeft(r.toSet)((ps, x) => if (ps(x)) ps -- (x * x to LIMIT by x) else ps))

        val primes = Stream.cons(2, Stream.from(3, 2) filter { _.isPrime })
    
        // Problem32
        def gcd(a: Int, b: Int): Int = if(b == 0) a else gcd(b, a % b)

        def main(args: Array[String]) {
            // Problem31
            println(7.isPrime)
            println(7.isPrime2)

            // Problem32
            println(gcd(36, 63))

            // Problem33
            println(35.isCoprimeTo(64))
        
            // Problem34
            println(10.totient)
        
            // Problem35
            println(315.primeFactors)

            // Problem36
            println(315.primeFactorsMult)
        }
    }
}
