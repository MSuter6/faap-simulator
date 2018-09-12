import scala.util.Random

package object Utils {
  /**
    * @param block Code block which is executed
    * @tparam R Return type of code block
    * @return Returned object of code block and used time
    */
  def time[R](block: => R): (R, Long) = {
    val start = System.nanoTime()
    val result = block
    val end = System.nanoTime()
    (result, end - start)
  }

  /**
    * Console output coloring according to boolean
    * @param bool Condition which
    * @return Colored console string
    */
  def boolColor(bool: Boolean): String ={
    val color = if(!bool) Console.GREEN else  Console.RED
    color + bool + Console.RESET
  }

  /**
    * Samples a gaussian according to provided mean and variance
    */
  def drawGaussian(rnd: Random, mean: Double, variance: Double = 1): Int = (rnd.nextGaussian() * math.sqrt(variance) + mean).toInt

  def atLeast(number: Int, shouldBeAtLeast: Int): Int = math.max(number, shouldBeAtLeast)
  def atLeast(number: Double, shouldBeAtLeast: Double): Double = math.max(number, shouldBeAtLeast)
  def atMost(number: Int, shouldBeAtMost: Int): Int = math.min(number, shouldBeAtMost)

  def clip(number: Int, min: Int, max: Int): Int = math.max(min, math.min(max, number))

  /**
    * Checks if expression is true, if not, a warning is printed on the console.
   */
  def check(expression: Boolean, msg: String): Unit = {
    if(!expression) {
      warning(msg)
    }
  }

  /**
    * Prints a colored warning to the console
    */
  def warning(msg: String): Unit = {
    println(s"${Console.YELLOW}[Warning]:\t$msg${Console.RESET}")
  }

  import java.io._

  /**
    * Simple serializer class which writes a serialized class to a file and makes use of Java Object-Streams.
    */
  object Serializer {
    def serialize[T <: Serializable](obj: T, path: String): Unit = {
      val oos = new ObjectOutputStream(new FileOutputStream(path))
      oos.writeObject(obj)
      oos.close()
    }

    def deserialize[T <: Serializable](path: String): T = {
      val ois = new ObjectInputStream(new FileInputStream(path))
      val input = ois.readObject.asInstanceOf[T]
      ois.close()
      input
    }
  }

  /**
    * Draws a random element from the given sequence.
    * @param seq Input sequence, If the sequence is empty, the behaviour is undefined
    * @param random Uses random from Global config if no implicit random available
    * @return The drawn element
    */
  def sampleFromSeq[N](seq: Seq[N])(implicit random: Random = GlobalConfig.random): N = {
    seq(random.nextInt(seq.size))
  }

  /**
    * Draws a random element from the given set.
    * @param set Input set, If the sequence is empty, the behaviour is undefined
    * @param random Uses random from Global config if no implicit random available
    * @return The drawn element
    */
  def sampleFromSet[N](set: Set[N])(implicit random: Random = GlobalConfig.random): N = {
    set.iterator.drop(random.nextInt(set.size)).next
  }

  /**
    * Samples an integer from a given range
    * @param start start of range, inclusive
    * @param end, end of range, exclusive
    * @param random random from Global config if no implicit random available
    * @return drawn integer
    */
  def sampleFromRange(start: Int, end: Int)(implicit random: Random = GlobalConfig.random): Int = start + random.nextInt(end-start)

  def roundToPrecision(number: Double, precision: Int = GlobalConfig.doublePrecision): Double = f"$number%1.4f".toDouble
  def importBigDecimalToPrecision(number: String, precision: Int = GlobalConfig.doublePrecision): BigDecimal = BigDecimal(number).setScale(precision, BigDecimal.RoundingMode.HALF_UP)
}
