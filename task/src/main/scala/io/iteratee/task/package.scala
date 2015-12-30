package io.iteratee

import scalaz.concurrent.Task

package object task extends Module[Task] with TaskOperations with TaskInstances
