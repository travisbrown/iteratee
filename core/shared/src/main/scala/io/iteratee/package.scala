package io

import cats.{ Eval, Id }

package object iteratee {
  type PureStep[E, A] = Step[E, Id, A]
  type PureIteratee[E, A] = Iteratee[E, Id, A]
  type PureEnumerator[E] = Enumerator[E, Id]
  type PureEnumeratee[O, I] = Enumeratee[O, I, Id]
}

package iteratee {
  final object pure extends Module[Id]
  final object eval extends Module[Eval]
  final object option extends Module[Option]
}
