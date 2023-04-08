package util

import java.io.{File, FileWriter}
import scala.annotation.tailrec
import scala.io.Source

//NU MODIFICA: functii utile
object Util {
  type Image = List[List[Pixel]]
  type GrayscaleImage = List[List[Double]]

  def toLinear(int: Integer): Double = {
    val normalised: Double = int / 255.0
    if (normalised <= 0.04045)
      normalised / 12.92
    else
      Math.pow((normalised + 0.055) / 1.055, 2.4)
  }

  def toGrayScale(pixel: Pixel) : Double = {
    val rL = toLinear(pixel.red)
    val gL = toLinear(pixel.green)
    val bL = toLinear(pixel.blue)
    0.2126 * rL + 0.7152 * gL + 0.0722 * bL
  }

  // functie care calculeaza vecinii n x n al fiecarui pixel
  def getNeighbors(image: GrayscaleImage, radius : Int) : List[List[GrayscaleImage]] = {
    val diameter = radius * 2 + 1

    @tailrec
    def getRow(rows : GrayscaleImage, acc : List[GrayscaleImage]) : List[GrayscaleImage] = {
      if (rows.head.length < diameter)
        acc.reverse
      else
        getRow(rows.map(_.drop(1)), rows.map(_.take(diameter)) :: acc)
    }

    @tailrec
    def getRows(partialImage : GrayscaleImage, acc : List[List[GrayscaleImage]]) : List[List[GrayscaleImage]] = {
      if (partialImage.length < diameter)
        acc.reverse
      else
        getRows(partialImage.drop(1), getRow(partialImage.take(diameter), Nil) :: acc)
    }

    getRows(image, Nil)
  }

  // functie care citeste fisierul intreg si il pune in string
  def readEntire(file: String): String = {
    val src: Source = Source.fromFile(file)
    val result: String = src.mkString
    src.close()
    result
  }

  // functie care verifica rezultatele cu fisierul corect
  def verifyResult(result: String, correctFile: String): (Boolean, String) = {
    val src : Source = Source.fromFile("correct/" + correctFile)
    val correct = src.mkString
    (correct.equals(result), "\nFailed for file " + correctFile)
  }

  // functie care scrie string-uri in fisierele de output
  def print(s: String, fileName: String): Unit = {
    val directory = new File("output")
    if (!directory.exists())
      directory.mkdir()
    val out: File = new File("output/" + fileName)
    val fw: FileWriter = new FileWriter(out, false)
    fw.write(s)
    fw.flush()
    fw.close()
  }
}
