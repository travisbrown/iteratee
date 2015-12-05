package io

import cats.{ Eval, Id }

package object iteratee

package iteratee {
  final object pure extends Module[Id]
  final object eval extends Module[Eval]
  final object option extends Module[Option]
}
