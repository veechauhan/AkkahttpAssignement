import java.io._
import akka.actor.typed.ActorSystem
import akka.actor.typed.scaladsl.Behaviors
import akka.http.scaladsl.Http
import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import akka.http.scaladsl.server.Directives._
import spray.json.{DefaultJsonProtocol, RootJsonFormat}
import scala.annotation.tailrec
import scala.io.Source
import scala.util.{Failure, Success, Try}

trait JsonSupport extends SprayJsonSupport with DefaultJsonProtocol {
  final case class GetFibonacci(number: Int, result: BigInt)
  implicit val jsonFormatting: RootJsonFormat[GetFibonacci] = jsonFormat2(GetFibonacci)

  final case class FileSupport(fileName: String, fileContent: String)
  implicit val FileJsonFormatting: RootJsonFormat[FileSupport] = jsonFormat2(FileSupport)

  final case class GetFile(fileName: String)
  implicit val GetFileJson: RootJsonFormat[GetFile] = jsonFormat1(GetFile)

  final case class DeleteFile(fileName: String)
  implicit val deleteFile: RootJsonFormat[DeleteFile] = jsonFormat1(DeleteFile)

  final case class RenameFile(fileName: String, newFileName: String)
  implicit val renameFile: RootJsonFormat[RenameFile] = jsonFormat2(RenameFile)

  final case class Status(status: Boolean)
  implicit val statusJson: RootJsonFormat[Status] = jsonFormat1(Status)


}

object AkkaHTTP extends JsonSupport with App {

  implicit val system: ActorSystem[Nothing] = ActorSystem(Behaviors.empty, "my-system")
  val route = {
      path("fibonacci") {
      (get & parameters("number")) { number =>
        val getNumber= Try(number.toInt)
        getNumber match {
          case Success(value)=>complete(GetFibonacci(number.toInt,FibonacciTail(value)))
          case Failure(e)=>complete(e+" (Please enter Integer no.)")
        }
      }
    } ~
      path("fileSaveFileAtLocation") {
        (post & entity(as[FileSupport])) { fileData =>
          val fileObject = new File(fileData.fileName)
          val printWriter = new PrintWriter(fileObject)
          printWriter.write(fileData.fileContent)
          printWriter.close()
          val response: Boolean = new java.io.File(fileData.fileName).isFile
          complete(Status(response))

        }
      } ~
      path("fileGetFileContent") {
        (post & entity(as[GetFile])) { file =>
          val fileTemp = new File(file.fileName)
          if (fileTemp.exists) {
            val fileSourceTemp = Source.fromFile(fileTemp)
            val fileSource = fileSourceTemp.mkString

            complete(FileSupport(fileTemp.toString, fileSource))
          } else {
            complete("File doesn't exists!")
          }

        }
      } ~
      path("fileDelete") {
        (delete & parameters("fileName")) { fileName =>
          val fileTemp = new File(fileName)
          if (fileTemp.exists) {
            fileTemp.delete()
            complete(Status(!fileTemp.exists()))
          }
          else
            complete("File doesn't exists.")
        }
      } ~
      path("fileRename") {
        (put & entity(as[RenameFile])) { file =>
          val fileTemp = new File(file.fileName)
          val NewFileTemp = new File(file.newFileName)
          if (fileTemp.exists) {
            fileTemp.renameTo(NewFileTemp)
            complete(Status(NewFileTemp.exists()))
          }
          else
            complete("File doesn't exists.")

        }
      }
  }

  Http().newServerAt("localhost", 8000).bind(route)
  println("If you want to find fibonacci number Click here and enter the number in last: http://localhost:8000/fibonacci?number=")
  def FibonacciTail(n: BigInt): BigInt = {
    @tailrec
    def FibonacciTailRec(i: BigInt, last: BigInt, nextToLast: BigInt): BigInt = {
      if (i >= n) last
      else FibonacciTailRec(i + 1, last + nextToLast, last)
    }

    if (n < 1) 0
    else FibonacciTailRec(2, 1, 1)
  }
}

