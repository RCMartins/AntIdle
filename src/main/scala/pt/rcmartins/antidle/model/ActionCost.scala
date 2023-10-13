package pt.rcmartins.antidle.model

case class ActionCost(
    sugar: Long = 0L,
    buildPower: Long = 0L,
)

object ActionCost {

  val empty: ActionCost = ActionCost()

}
