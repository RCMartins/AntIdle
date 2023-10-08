package pt.rcmartins.antidle.model

sealed trait AntBrood {

  def isEgg: Boolean
  def isLarvae: Boolean
  def isPupae: Boolean

}

object AntBrood {

  case class Egg(
      tickOfEgg: Long,
  ) extends AntBrood {
    val isEgg: Boolean = true
    val isLarvae: Boolean = false
    val isPupae: Boolean = false
  }

  case class Larvae(
      tickOfLarvae: Long,
  ) extends AntBrood {
    val isEgg: Boolean = false
    val isLarvae: Boolean = true
    val isPupae: Boolean = false
  }

  case class Pupae(
      tickOfPupae: Long,
  ) extends AntBrood {
    val isEgg: Boolean = false
    val isLarvae: Boolean = false
    val isPupae: Boolean = true
  }

  val defaultTicksToLarva: Long = 50

}
