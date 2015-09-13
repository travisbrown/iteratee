package io.iteratee

import cats.{ Comonad, Functor, Monad }
//import java.util.concurrent.{ CompletableFuture, CompletionStage, Executor }
//import java.util.function.{ Function => JFunction, Supplier => JSupplier }

/*trait Future[A] extends CompletionStage[A] { self =>

}*/

/*class Future[A](a: => A) extends CompletableFuture[A] { self =>
  val supplier: Supplier[A] = new Supplier[A] {
    def get(): A = a
  }

  override def get(): A = {
    supplyAsync(supplier).thenAccept(
      new Consumer[A] {
        def accept(a: A): Unit = self.complete
      }
    )
  }
}*/

package object future {
  /*implicit val futureMonad: Monad[CompletableFuture] = new Monad[CompletableFuture] {
    def pure[A](x: A): CompletableFuture[A] = CompletableFuture.completedFuture(x)

    override def map[A, B](fa: CompletableFuture[A])(f: A => B): CompletableFuture[B] =
      fa.thenApply(
        new JFunction[A, B] {
          def apply(a: A): B = f(a) 
        }
      )

    def flatMap[A, B](fa: CompletableFuture[A])(f: A => CompletableFuture[B]): CompletableFuture[B] =
      fa.thenCompose(
        new JFunction[A, CompletableFuture[B]] {
          def apply(a: A): CompletableFuture[B] = f(a)
        }
      )
  }

  def run[A](fa: CompletableFuture[A])(executor: Executor): A = fa match {
    case f: Future[A] => 
  }

  def create[A](a: => A): CompletableFuture[A] = new CompletableFuture[A]
  */
}
