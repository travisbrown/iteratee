package io.iteratee

/**
 * Internal types and utilities
 *
 * While some of these types and utilities are part of the public API and are
 * used in the implementation of [[io.iteratee]], they are not designed for
 * clarity or ease-of-use, and should never be needed for idiomatic use of the
 * library.
 */
package object internal {
  /**
   * Fail because of a divergent iteratee
   *
   * It is possible to construct an [[Iteratee]] (manually) that does not enter
   * the completed state after receiving an [[Input.end]]. We call these
   * iteratees "divergent", and operations on these iteratees may fail with the
   * following error.
   *
   * None of the iteratees provided by the library are divergent, and the API is
   * designed to discourage the construction of divergent iteratees, but having
   * the compiler verify this is inconvenient.
   */
  private[iteratee] final def diverge[A]: A = sys.error("Divergent iteratee")
}
