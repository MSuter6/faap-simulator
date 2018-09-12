import scala.util.Random

/**
  * Holds global config values.
  */
package object GlobalConfig {
  private var privRandomSeed = 54351321


  def seed: Int = privRandomSeed

  def setRandomSeed(seed: Int): Unit = {
    privRandomSeed = seed
    randomPriv = new Random(seed)
  }

  private var randomPriv = new Random(seed)

  /**
    * @return Random object which is used as a basis for all random operations used in the simulator
    */
  def random: Random = randomPriv

  val NL = sys.props("line.separator")

  val graphNodeCountDrawingThreshold = 35

  val expTimestamp = "dd-MM-yyyy.HH-mm-ss"

  val doublePrecision = 4

  val zimplLimit = 400
}
