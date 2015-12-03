package io

import cats.{ Eval, Id }

package object iteratee {
  type PureStep[E, A] = Step[E, Id, A]
  type PureIteratee[E, A] = Iteratee[E, Id, A]
  type PureEnumerator[E] = Enumerator[E, Id]
  type PureEnumeratee[O, I] = Enumeratee[O, I, Id]
}

package iteratee {
  object pure extends Module[Id]
  object eval extends Module[Eval]
  object option extends Module[Option]
}
