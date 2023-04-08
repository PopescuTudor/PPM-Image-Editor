import util.Pixel
import util.Util

import scala.annotation.tailrec

// Online viewer: https://0xc0de.fr/webppm/
object Solution extends App {
  type Image = List[List[Pixel]]
  type GrayscaleImage = List[List[Double]]

  // prerequisites
  def fromStringPPM(image: List[Char]): Image = {
    @tailrec
    def drop(n: Int, l: List[List[Char]]): List[List[Char]] =
      if (n == 0) l
      else
        l match {
          case Nil => Nil
          case x :: xs => drop(n - 1, xs)
        }

    def split(delim: Char)(s: List[Char]): List[List[Char]] = {
      def op(c: Char, acc: List[List[Char]]): List[List[Char]] =
        acc match {
          case Nil => if (c == delim) Nil else List(List(c))
          case x :: xs => if (c == delim) Nil :: acc else (c :: x) :: xs
        }

      s.foldRight(Nil: List[List[Char]])(op)
    }

    def listInttoPixel(y: List[Int]): Pixel =
      y match {
        case Nil => Pixel(0, 0, 0)
        case x :: xs => Pixel(x, xs.head, xs.tail.head)
      }

    def listChartoInt(l: List[Char]): List[Int] = {
      def op(l: List[Char]): Int =
        l match {
          case Nil => 0
          case x :: Nil => x.asDigit
          case x :: xs =>
            xs match {
              case y :: Nil => x.asDigit * 10 + y.asDigit
              case y :: ys => x.asDigit * 100 + y.asDigit * 10 + ys.head.asDigit
            }
        }

      split(' ')(l).map(op)
    }

    val len: Int = listChartoInt(split('\n')(image).tail.head).head
    drop(3, split('\n')(image)).map(listChartoInt).map(listInttoPixel).grouped(len).toList
  }

  def toStringPPM(image: Image): List[Char] = {
    val height: Int = image.size
    val len: Int = image.head.size

    def getDigits(n: Int): List[Char] = {
      @tailrec
      def op(n: Int, digits: List[Char]): List[Char] = {
        if (n < 10) (n + '0').toChar :: digits else op(n / 10, (n % 10 + '0').toChar :: digits)
      }

      op(n, Nil)
    }

    def createHeader(height: Int, len: Int): List[Char] =
      List('P', '3', '\n') ::: getDigits(len) ::: (' ' :: getDigits(height)) ::: List('\n', '2', '5', '5', '\n')

    createHeader(height, len) ::: image.flatten.flatMap(pixel =>
      getDigits(pixel.red) ::: (' ' :: getDigits(pixel.green)) ::: (' ' :: getDigits(pixel.blue)) ::: List('\n'))
  }

  // ex 1
  def verticalConcat(image1: Image, image2: Image): Image =
    image1 ::: image2

  // ex 2
  def horizontalConcat(image1: Image, image2: Image): Image =
    image1.zip(image2).map(x => x._1 ::: x._2)

  // ex 3
  def rotate(image: Image, degrees: Integer): Image =
    degrees % 360 match {
      case 0 => image
      case 90 => image.transpose.reverse
      case 180 => image.reverse.map(_.reverse)
      case 270 => image.transpose.map(_.reverse)
      case _ => throw new IllegalArgumentException("degrees must be a multiple of 90")
    }

  // ex 4
  val gaussianBlurKernel: GrayscaleImage = List[List[Double]](
    List(1, 4, 7, 4, 1),
    List(4, 16, 26, 16, 4),
    List(7, 26, 41, 26, 7),
    List(4, 16, 26, 16, 4),
    List(1, 4, 7, 4, 1)
  ).map(_.map(_ / 273))

  val Gx: GrayscaleImage = List(
    List(-1, 0, 1),
    List(-2, 0, 2),
    List(-1, 0, 1)
  )

  val Gy: GrayscaleImage = List(
    List(1, 2, 1),
    List(0, 0, 0),
    List(-1, -2, -1)
  )

  def absoluteValue(x: Double): Double = if (x < 0) -x else x

  def edgeDetection(image: Image, threshold: Double): Image = {
    val grayscaleImage: GrayscaleImage = image.map(_.map(Util.toGrayScale))
    val conv = applyConvolution(grayscaleImage, gaussianBlurKernel)
    val neighbors = Util.getNeighbors(conv, 1)

    val mx = neighbors.map(_.map(_.zip(Gx).map(x => x._1.zip(x._2).map(y => y._1 * y._2).sum).sum))
    val my = neighbors.map(_.map(_.zip(Gy).map(x => x._1.zip(x._2).map(y => y._1 * y._2).sum).sum))
    val m: GrayscaleImage = mx.zip(my).map(line => line._1.zip(line._2).map(pair => absoluteValue(pair._1) + absoluteValue(pair._2)))

    m.map(_.map(x => if (x >= threshold) Pixel(255, 255, 255) else Pixel(0, 0, 0)))
  }

  def applyConvolution(image: GrayscaleImage, kernel: GrayscaleImage): GrayscaleImage = {
    val rad = (kernel.size - 1) / 2
    val neighbors = Util.getNeighbors(image, rad)
    neighbors.map(_.map(_.zip(kernel).map(x => x._1.zip(x._2).map(y => y._1 * y._2).sum).sum))
  }

  // ex 5
  def moduloPascal(m: Integer, funct: Integer => Pixel, size: Integer): Image = {
    val firstLine = List(1 :: (1 until size).map(_ => 0).toList)
    @tailrec
    def recursion(acc: List[List[Int]], count: Integer): List[List[Int]] = {
      if (count == size) acc
      else {
        val prev = acc.last
        val next: List[Int]= 1 :: (1 until size).map(i => (prev(i) + prev(i - 1)) % m).toList
        recursion(acc ::: List(next), count + 1)
      }
    }

    val pascal = recursion(firstLine, 1)

    def applyfunctOnlyBelowDiagonal(i: Integer, j: Integer): Pixel = {
      if(i > size || j > size || i < 0 || j < 0) Pixel(0, 0, 0)
      else if (i >= j) funct(pascal(i)(j)) else Pixel(0, 0, 0)
    }
    pascal.zipWithIndex.map(x => x._1.zipWithIndex.map(y => applyfunctOnlyBelowDiagonal(x._2, y._2)))
  }

}
