package pt.rcmartins.antidle.model

case class Unlocks(
    canLayEggs: Boolean = false,
    layedFirstEgg: Boolean = false,
    bornFirstWorker: Boolean = false,
    antTasksUnlocked: Boolean = false,
    canUpgradeNest: Boolean = false,
    larvaeUnlocked: Boolean = false,
    pupaeUnlocked: Boolean = false,
)
