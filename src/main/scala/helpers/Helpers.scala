package helpers

import zio.*
import zio.ZIO.*
import java.nio.file.{Path, Files}
import java.nio.charset.Charset

object Helpers {

  def readResourceContent(resourceName: String, charsetName: String = "UTF-8") = {
    for { // KO to enhance
      inputStream <- attempt(Helpers.getClass.getResourceAsStream(resourceName))
      bytes       <- attemptBlockingIO(inputStream.readAllBytes())
      content     <- attempt(new String(bytes, charsetName))
    } yield content
  }

  def readPathContent(inputPath: Path, charsetName: String = "UTF-8") = for {
    charset <- attempt(Charset.forName(charsetName))
    content <- attemptBlockingIO(Files.readString(inputPath, charset))
  } yield content

  def readFileContent(filename: String, charsetName: String = "UTF-8") = for {
    inputPath <- attempt(Path.of(filename))
    content   <- readPathContent(inputPath, charsetName)
  } yield content

  def writePathContent(outputPath: Path, content: String, charsetName: String = "UTF-8") = for {
    charset <- attempt(Charset.forName(charsetName))
    _       <- attemptBlockingIO(Files.writeString(outputPath, content, charset))
  } yield ()

  def writeFileContent(filename: String, content: String, charsetName: String = "UTF-8") = for {
    outputPath <- attempt(Path.of(filename))
    _          <- writePathContent(outputPath, content, charsetName)
  } yield ()
}
