package io.iteratee.internal

import java.util.NoSuchElementException
import scala.collection.AbstractIterator
import scala.collection.mutable.Builder

private[iteratee] abstract class Chunk[A] {
	def head1: A
	def head2: A
	def tail: Iterator[A]
	def last: A

	def size: Int

	def prepend(a: A): Chunk[A]
	def append(a: A): Chunk[A]
	def concat(other: Chunk[A]): Chunk[A]

	def newBuilder(head1: A, head2: A): Builder[A, Chunk[A]]

	final def toList: List[A] = head1 :: head2 :: tail.toList
	def toVector: Vector[A]
}

private[iteratee] object Chunk {
	private[this] final class VectorChunk[A](value: Vector[A]) extends Chunk[A] {
		final def head1: A = value.head
		final def head2: A = value(1)
		final def tail: Iterator[A] = value.iterator.drop(2)
		final def last: A = value.last

		final def size: Int = value.size

		final def prepend(a: A): Chunk[A] = new VectorChunk(a +: value)
		final def append(a: A): Chunk[A] = new VectorChunk(value :+ a)
	  final def concat(other: Chunk[A]): Chunk[A] = new VectorChunk(value ++ other.toVector)

		final def newBuilder(head1: A, head2: A): Builder[A, Chunk[A]] = {
			val builder = Vector.newBuilder[A].mapResult(new VectorChunk(_))
			builder += head1
			builder += head2
			builder
		}

	  final def toVector: Vector[A] = value
	}

	private[this] final val emptyIterator: Iterator[Any] = new AbstractIterator[Any] {
    final def hasNext: Boolean = false
    final def next(): Any = throw new NoSuchElementException("next on empty iterator")
	}

	final def apply[A](head1: A, head2: A): Chunk[A] = new Chunk[A] {
		final def head1: A = head1
		final def head2: A = head2
		final def tail: Iterator[A] = emptyIterator.asInstanceOf[Iterator[A]]
		final def last: A = head2
		final def size: Int = 2
		final def prepend(a: A): Chunk[A] = new VectorChunk(a +: head1 +: head2 +: Vector.empty)
		final def append(a: A): Chunk[A] = new VectorChunk(head1 +: head2 +: a +: Vector.empty)

		final def newBuilder(head1: A, head2: A): Builder[A, Chunk[A]] = {
			val builder = Vector.newBuilder[A].mapResult(new VectorChunk(_))
			builder += head1
			builder += head2
			builder
		}

	  final def toVector: Vector[A] = Vector(head1, head2)
	}

	final def fromSeqUnsafe[A](values: Seq[A]): Chunk[A] = new VectorChunk(values.toVector)
}