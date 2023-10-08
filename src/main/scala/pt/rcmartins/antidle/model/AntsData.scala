package pt.rcmartins.antidle.model

case class AntsData(
    eggsAndLarvae: Seq[AntBrood],
    workers: Long,
    tasks: Seq[(AntTask, Long)],
) {

  val idleWorkersCount: Long =
    workers - tasks.map(_._2).sum

  def unlockTask(anttask: AntTask): AntsData =
    if (tasks.exists(_._1 == anttask))
      this
    else
      copy(tasks = tasks :+ (anttask, 0))

}

object AntsData {

  val initial: AntsData =
    AntsData(
      eggsAndLarvae = Seq.empty,
      workers = 0,
      tasks = Seq.empty,
    )

}
