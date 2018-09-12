package simulator.allocation

class AllocationException(msg: String) extends Exception(msg) {
  def this(msg: String, cause: Throwable) {
    this(msg)
    initCause(cause)
  }

  def this(cause: Throwable) {
    this(Option(cause).map(_.toString).orNull, cause)
  }

  def this() {
    this(null: String)
  }
}

class HeuristicsFailedToFindSolutionException(private val message: String = "",
                                              private val cause: Throwable = None.orNull) extends AllocationException(message, cause)

class LpSolverFailedToFindSolutionException(private val message: String = "",
                                            private val cause: Throwable = None.orNull) extends AllocationException(message, cause)
