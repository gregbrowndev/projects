package com.rockthejvm.jobsboard.core.application.ports.out.laws.discipline

import cats.Eq
import cats.MonadThrow
import cats.laws.IsEq
import cats.syntax.all.*
import org.scalacheck.Arbitrary
import org.scalacheck.Shrink
import org.scalacheck.effect.PropF

trait LawAdapter {

  def booleanPropF[F[_]: MonadThrow, A](propLabel: String, prop: => Boolean): (String, PropF[F]) =
    propLabel -> PropF.boolean(prop)

  // TODO - could be cleaner to add something similar as an extension method to PropF
  // e.g. forAllF ( f: A => IsEq[F[_]] ): PropF[F]
  // or   forAllF ( f: A => F[IsEq[_]] ): PropF[F]

  def isEqPropF[F[_], A: Arbitrary: Shrink, B: Eq](propLabel: String, prop: A => IsEq[F[B]])(
      implicit F: MonadThrow[F]
  ): (String, PropF[F]) =
    propLabel -> PropF
      .forAllF { (a: A) =>
        val isEq = prop(a)
        (isEq.lhs, isEq.rhs).mapN(_ === _).flatMap(b => F.catchOnly[AssertionError](assert(b)))
      }
      .map(p => p.copy(labels = p.labels + propLabel))

}

object LawAdapter extends LawAdapter