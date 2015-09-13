package io.travisbrown

import cats.Id

package object iteratee {
  type PureStep[E, A] = Step[E, Id, A]
  type PureIteratee[E, A] = Iteratee[E, Id, A]
  type PureEnumerator[E] = Enumerator[E, Id]
  type PureEnumeratee[O, I] = Enumeratee[O, I, Id]
}
