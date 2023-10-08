package pt.rcmartins.antidle.model

case class BasicResources(
    sugars: Long,
    proteins: Long,
    water: Long,
    colonyPoints: Long, // Needs a better name? Biomass? Nectar? Queen's Favor?
    DNA: Long,
)

object BasicResources {

  val initial: BasicResources =
    BasicResources(
      sugars = 0,
      proteins = 0,
      water = 0,
      colonyPoints = 0,
      DNA = 0,
    )

}
