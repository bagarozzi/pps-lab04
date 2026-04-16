package it.unibo.pps.tasks.typeclasses

import it.unibo.pps.u03.Sequences.Sequence
import Sequence.*
import it.unibo.pps.u03.Optionals.Optional

/*  Exercise 5: 
 *  - Generalise by ad-hoc polymorphism logAll, such that:
 *  -- it can be called on Sequences but also on Optional, or others... 
 *  -- it does not necessarily call log, but any function with analogous type
 *  - Hint: introduce a type class Traversable[T[_]]], capturing the ability of calling a
 *    "consumer function" on all elements (with type A) of a datastructure T[A] 
 *    Note Traversable is a 2-kinded trait (similar to Filterable, or Monad)
 *  - Write givens for Traversable[Optional] and Traversable[Sequence]
 *  - Show you can use the generalisation of logAll to:
 *  -- log all elements of an Optional, or of a Traversable
 *  -- println(_) all elements of an Optional, or of a Traversable
 */

object Ex5Traversable:

  trait Traversable[T[_]]:
      def consume[A](ta: T[A])(cons: A => Unit): Unit

  given Traversable[Sequence] with
    def consume[A](seq: Sequence[A])(cons: A => Unit): Unit = seq match
      case Cons(h, t) => cons(h) ; consume(t)(cons)
      case _ => ()

  given Traversable[Optional] with
    def consume[A](opt: Optional[A])(cons: A => Unit): Unit = opt match
      case Optional.Just(a) => cons(a)
      case Optional.Empty() => ()

  private def log[A](a: A): Unit = println("The next element is: "+a)

  private def logAll[F[_]: Traversable, A](data: F[A]): Unit = summon[Traversable[F]].consume(data)(log)

  @main def TestTraversable(): Unit =
    val s = Cons("1", Cons("2", Cons("3", Nil())))
    logAll(s)
    val o = Optional.Just(4)
    val emptyO = Optional.Empty()
    logAll(o)
    logAll(emptyO)