package comp

class Comp[A,B](val f:A=>B)extends AnyVal{
  private[this] def composition[A,B,C](f1:A=>B)(f2:B=>C):A=>C = a=> f2(f1(a))
  def <=:[C](f1:B=>C) = new Comp[A,C](composition(f)(f1))
  def :=>[C](f1:B=>C) = new Comp[A,C](composition(f)(f1))
  def apply(sv:A):B = f(sv)
  def function = f
}
object Comp{
  def <=:[A,B](f:A=>B) = new Comp[A,B](f)
  def :=>[A,B](f:A=>B) = new Comp[A,B](f)
}
object Main extends App{
  val f1 = (a:Int)=>a+1
  val f2 = (a:Int)=>a.toString
  val f3: String=>Unit = a => println(a)
  val f4 = (f3<=:f2<=:f1<=:Comp).function
  val f5 = (Comp:=>f1:=>f2).function
}

//case object Fin extends :~:()