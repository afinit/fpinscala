def gcd(x: Long, y: Long): Long = {
  if (x < y) gcd(y,x)
  else if (y == 0) x
  else gcd(y, Math.floorMod(x, y))
}

gcd(5,3)
gcd(18,98)
gcd(9234942,112314)
gcd(0,59)
gcd(23,77)

def extEuclid(x: Long, y: Long): (Long, Long, Long) = {
  if (y == 0) (x, 1, 0)
  else {
    val (d, aPrime, bPrime) = extEuclid(y, Math.floorMod(x,y))
    (d, bPrime, aPrime - x / y * bPrime)
  }
}

def pow(x: Long, y: Long): Long = scala.math.pow(x,y).toLong
def mod(x: Long, y: Long): Long = Math.floorMod(x,y)
//def powMod(x: )
pow(2,3)
mod(4,5)

extEuclid(4, 13)
extEuclid(360, 7)
extEuclid(7, 360)
extEuclid(21,91)
Math.floorMod(-4,91)
Math.floorMod(87*21, 91)

extEuclid(13, 13)
extEuclid(3,28)
extEuclid(7, 60)
gcd(7,60)
Math.floorMod(-17, 60)
val r1 = Math.floorMod( pow(23, 7), 77)
pow(23,7)
Math.floorMod( pow(r1, 43), 77)

gcd(13,60)
extEuclid(13, 60)
Math.floorMod(-23, 60)
val r2 = Math.floorMod(23^13, 77)
Math.floorMod(r2^37, 77)
2^2

Math.floorMod(3 * -9, 28)
Math.floorMod(3 * 19, 28)

def checkRelativePrime(x: Int, y: Int): Boolean = {
  gcd(x,y) == 1
}

val N = 19
(1 until N).toVector
  .collect {case x if gcd(x, N) == 1 => x}
  .map(x => (Math.floorMod(extEuclid(x, N)._2, N), x))
  .map { case (x,y) => (x * y) % N }
  .length

(25*8) % 28
