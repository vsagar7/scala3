class Rational(n: Int, d: Int) {
 require(d != 0)
 //Defining constructor
 def this(n: Int) = this(n, 1) // auxiliary constructor to support single arg
 def this() = this(1, 1) // auxiliary constructor to support no arg
private val g = gcd(n.abs, d.abs)
 val numer = n / g
 val denom = d / g
 //Add for Rational numbers
 def + (that: Rational): Rational =
 new Rational(
 numer * that.denom + that.numer * denom,
 denom * that.denom
 )
 //Overloaded Add for whole numbers
 def + (i: Int): Rational =
 new Rational(numer + i * denom, denom)
 //Subract for Rational numbers
 def - (that: Rational): Rational =
 new Rational(
 numer * that.denom - that.numer * denom,
 denom * that.denom
 )
 //Overloaded Subract for whole numbers
 def - (i: Int): Rational =
 new Rational(numer - i * denom, denom)
 //Multiply for Rational numbers
 def * (that: Rational): Rational =
 new Rational(numer * that.numer, denom * that.denom)
 //Overloaded Multiply for whole numbers
 def * (i: Int): Rational =
 new Rational(numer * i, denom)
 //Div for Rational numbers
 def / (that: Rational): Rational =
 new Rational(numer * that.denom, denom * that.numer)
 //Overloaded Div for whole numbers
 def / (i: Int): Rational =
 new Rational(numer, denom * i)
 //GCD
 private def gcd(a: Int, b: Int): Int =
 if (b == 0) a else gcd(b, a % b)
 //overridden To String method, to print meaningful output
 override def toString = numer + "/" + denom
}
object assignment16 extends App {
//rational number input
 val r1 = new Rational(8, 3) 
 val r2 = new Rational(3, 4) 
 val r3 = new Rational(5, 7)
 println(r1 +" + "+ r2 +" = "+ (r1 + r2)); 
 println(r1 +" - "+ r2 +" = "+ (r1 - r2));
 println(r1 +" * "+ r2 +" = "+ (r1 * r2));
 println(r1 +" / "+ r2 +" = "+ (r1 / r2));
 println(r1+" == "+r2+" : " +(r1 == r2))
val x = new Rational(4, 3) 
 val y = new Rational(2, 7) 
 val z = new Rational(9, 2)
 println(x+" - "+y+" - "+z+" = " +(x - y - z)) 
 //passing whole number as rational number
 val a = new Rational(5)
 val b = new Rational
 println(a+" + "+b+" = "+(a+b));
}