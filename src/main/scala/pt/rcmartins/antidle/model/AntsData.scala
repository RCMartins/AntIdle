package pt.rcmartins.antidle.model

import pt.rcmartins.antidle.game.Constants.u
import zio.json._

case class AntsData(
    eggsAndLarvae: Seq[AntBrood],
    workers: Long,
    tasks: Seq[(AntTask, Long)],
    sugarCumulativeDebt: Long,
) {

  val idleWorkersCount: Long =
    workers - tasks.map(_._2).sum

  def unlockTask(antTask: AntTask): AntsData =
    if (tasks.exists(_._1 == antTask))
      this
    else
      copy(tasks = tasks :+ (antTask, 0))

  def remove1WorkerFromLastTask: AntsData =
    tasks.zipWithIndex.findLast(_._1._2 > 0) match {
      case None =>
        this
      case Some(((task, count), index)) =>
        val newCount = count - 1 * u
        copy(tasks = tasks.updated(index, (task, newCount)))
    }

}

object AntsData {

  implicit val decoder: JsonDecoder[AntsData] = DeriveJsonDecoder.gen[AntsData]
  implicit val encoder: JsonEncoder[AntsData] = DeriveJsonEncoder.gen[AntsData]

  val initial: AntsData =
    AntsData(
      eggsAndLarvae = Seq.empty,
      workers = 0,
      tasks = Seq.empty,
      sugarCumulativeDebt = 0,
    )

}
