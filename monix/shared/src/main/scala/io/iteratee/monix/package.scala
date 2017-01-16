package io.iteratee

/**
 * Module and instances for Monix.
 *
 * There are two ways to use this package. If your project has a monix-cats
 * dependency, you can import the type class instances it provides and then
 * instantiate a Monix module with `val module = TaskModule.instance`. If you do
 * not have a monix-cats dependency, you can use the `io.iteratee.monix.task`
 * module directly.
 */
package monix {
  final object task extends DefaultTaskModule
}
