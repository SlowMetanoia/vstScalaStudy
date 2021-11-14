package Composition

import scala.language.implicitConversions

class SimpleFunctionComposition[A,B] private(val f:A=>B)extends AnyVal{
  private[this] def composition[D,E,F](f1:D=>E)(f2:E=>F):D=>F = a=> f2(f1(a))
  def <=:[C](f1:B=>C) = new SimpleFunctionComposition[A,C](composition(f)(f1))
  def :=>[C](f1:B=>C) = new SimpleFunctionComposition[A,C](composition(f)(f1))
  def compose = f
}

object SimpleFunctionComposition{
  implicit def function2Comp[A,B](f:A=>B):SimpleFunctionComposition[A,B] = SimpleFunctionComposition(f)
  implicit def Comp2function[A,B](comp: SimpleFunctionComposition[A,B]):A=>B = comp.compose
  def apply[A, B](function: A => B): SimpleFunctionComposition[A, B] = new SimpleFunctionComposition(function)
}