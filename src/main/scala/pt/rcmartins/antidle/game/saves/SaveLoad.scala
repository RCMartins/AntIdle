package pt.rcmartins.antidle.game.saves

import org.scalajs.dom.window.localStorage
import pt.rcmartins.antidle.model.AllData
import zio.json._

object SaveLoad {

  private val SaveGameName = "save"

  def saveToLocalStorage(allData: AllData): Unit =
    localStorage.setItem(SaveGameName, saveString(allData))

  def saveString(allData: AllData): String =
    jsonToBase64(saveToJson(allData))

  private def saveToJson(allData: AllData): String =
    AllDataForSave.fromAllData(allData).toJson

  private def jsonToBase64(str: String): String =
    java.util.Base64.getEncoder.encodeToString(str.getBytes)

  def loadFromLocalStorage(): Option[AllData] =
    Option(localStorage.getItem(SaveGameName))
      .flatMap(loadString)
      .filter(_.version == AllData.CurrentVersion)

  def loadString(str: String): Option[AllData] = {
    val strTrimmed = str.trim
    if (strTrimmed.startsWith("{"))
      loadFromJson(strTrimmed)
    else
      loadFromJson(base64ToJson(strTrimmed))
  }

  private def loadFromJson(str: String): Option[AllData] =
    str.fromJson[AllDataForSave].toOption.map(_.toAllData)

  private def base64ToJson(str: String): String =
    new String(java.util.Base64.getDecoder.decode(str.getBytes))

}
