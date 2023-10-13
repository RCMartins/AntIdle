package pt.rcmartins.antidle.model

case class ActionBonus(
    sugar: Long = 0L,
    eggs: Long = 0L,
    colonyPoints: Long = 0L,
    maxWorkers: Long = 0L,
)

object ActionBonus {

  val empty: ActionBonus = ActionBonus()

}
