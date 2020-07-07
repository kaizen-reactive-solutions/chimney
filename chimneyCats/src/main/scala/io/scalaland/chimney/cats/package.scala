package io.scalaland.chimney

import _root_.cats.data._
import _root_.cats.kernel.Semigroup
import _root_.cats.{Applicative,Eval}

import scala.collection.compat._
import scala.collection.compat.immutable.{LazyList => CompatLazyList}

package object cats extends CatsImplicits

trait CatsImplicits extends LowPriorityImplicits {

  // normally, these instances are not needed, but few tests with .enableUnsafeOption fail to compile without them
  implicit def TransformerFValidatedNecSupport[E: Semigroup]: TransformerFSupport[ValidatedNec[E, +*]] =
    deriveTransformerFSupport[ValidatedNec[E, +*]]

  implicit def TransformerFValidatedNelSupport[E: Semigroup]: TransformerFSupport[ValidatedNel[E, +*]] =
    deriveTransformerFSupport[ValidatedNel[E, +*]]

  implicit def TransformerFIorNecSupport[E: Semigroup]: TransformerFSupport[IorNec[E, +*]] =
    deriveTransformerFSupport[IorNec[E, +*]]

  implicit def TransformerFIorNelSupport[E]: TransformerFSupport[IorNel[E, +*]] =
    deriveTransformerFSupport[IorNel[E, +*]]

  implicit def TransformerFIorNesSupport[E]: TransformerFSupport[IorNes[E, +*]] =
    deriveTransformerFSupport[IorNes[E, +*]]
}

trait LowPriorityImplicits {
  implicit def deriveTransformerFSupport[F[+_]](implicit F: Applicative[F]): TransformerFSupport[F] = new TransformerFSupport[F] {
    override def pure[A](value: A): F[A] = F.pure(value)

    override def product[A, B](fa: F[A], fb: => F[B]): F[(A, B)] = F.tuple2(fa, fb)

    override def map[A, B](fa: F[A], f: A => B): F[B] = F.map(fa)(f)

    override def traverse[M, A, B](it: Iterator[A], f: A => F[B])(implicit fac: Factory[B, M]): F[M] = {
      F.map(it.foldRight(Eval.always(F.pure(CompatLazyList.empty[B]))) { (next: A, acc: Eval[F[CompatLazyList[B]]]) =>
        F.map2Eval(f(next), acc) { (next: B, acc: CompatLazyList[B]) =>
          next #:: acc
        }
      }.value)(fac.fromSpecific(_))
    }
  }
}
//
//  implicit def TransformerFIorSupport[EE: Semigroup]: TransformerFSupport[Ior[EE, +*]] =
//    new TransformerFSupport[Ior[EE, +*]] {
//      override def pure[A](value: A): Ior[EE, A] =
//        Ior.right(value)
//
//      override def product[A, B](fa: Ior[EE, A], fb: => Ior[EE, B]): Ior[EE, (A, B)] =
//        fa.flatMap(a => fb.map(b => (a, b)))
//
//      override def map[A, B](fa: Ior[EE, A], f: A => B): Ior[EE, B] =
//        fa.map(f)
//
//      override def traverse[M, A, B](it: Iterator[A], f: A => Ior[EE, B])(implicit fac: Factory[B, M]): Ior[EE, M] =
//        it.foldRight(Eval.always(pure(CompatLazyList.empty[B]))) { (next: A, acc: Eval[Ior[EE, CompatLazyList[B]]]) =>
//            Apply[Ior[EE, *]].map2Eval(f(next), acc)(_ #:: _)
//          }
//          .value
//          .map(fac.fromSpecific(_))
//    }
//
//  implicit def TransformerFValidatedSupport[EE: Semigroup]: TransformerFSupport[Validated[EE, +*]] =
//    new TransformerFSupport[Validated[EE, +*]] {
//      def pure[A](value: A): Validated[EE, A] =
//        Validated.Valid(value)
//
//      def product[A, B](fa: Validated[EE, A], fb: => Validated[EE, B]): Validated[EE, (A, B)] =
//        fa.product(fb)
//
//      def map[A, B](fa: Validated[EE, A], f: A => B): Validated[EE, B] =
//        fa.map(f)
//
//      def traverse[M, A, B](it: Iterator[A], f: A => Validated[EE, B])(
//          implicit fac: Factory[B, M]
//      ): Validated[EE, M] = {
//        val (valid, invalid) = it.map(f).partition(_.isValid)
//        Semigroup[EE].combineAllOption(invalid.collect { case Validated.Invalid(e) => e }) match {
//          case Some(errors) =>
//            Validated.Invalid(errors)
//          case None =>
//            val b = fac.newBuilder
//            b ++= valid.collect { case Validated.Valid(b) => b }
//            Validated.Valid(b.result())
//        }
//      }
//    }
//}
