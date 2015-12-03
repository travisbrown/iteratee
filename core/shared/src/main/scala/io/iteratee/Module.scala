package io.iteratee

trait Module[F[_]] extends EnumerateeModule[F] with EnumeratorModule[F] with IterateeModule[F]
