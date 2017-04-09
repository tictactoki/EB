import java.io.{BufferedWriter, File, FileWriter}

import scala.io.Source
import scala.util.Try

/**
  * Created by stephane on 09/04/2017.
  */

trait RowDecoder[T] {
  def decode(row: IndexedSeq[String]): T
}

trait Parser[T] {

  implicit val decoder: RowDecoder[T]

  def parserCsv(source: Source)(implicit decoder: RowDecoder[T]) = {
    source.getLines().map(row => decoder.decode(row.split(",")))
  }
}

object XagParser extends Parser[(String, String, Float, Long)] {
  override implicit val decoder = new RowDecoder[(String, String, Float, Long)] {
    override def decode(row: IndexedSeq[String]): (String, String, Float, Long) = {
      (row(0), row(1), row(2).toFloat, row(3).toLong)
    }
  }

  // convert milliseconds to days
  val millisecondDay: Long = 1000 * 60 * 60 * 24

  /**
    *
    * @param max
    * @param current
    * @param rating
    * @param penalty
    * @return
    */
  protected def computeRating(max: Long, current: Long, rating: Float, penalty: Float = 0.95f) = {
    val diff = ((max - current) / millisecondDay).toInt
    if (diff > 0) (0 until diff).foldLeft(rating)((acc, value) => acc * penalty)
    else rating
  }

  /**
    *
    * @param name
    * @return
    */
  protected def getBufferedWriter(name: String) = {
    new BufferedWriter(new FileWriter("./" + name))
  }

  /**
    *
    * @param value
    * @param bufferedWriter
    */
  protected def write(value: String, bufferedWriter: BufferedWriter) = {
    bufferedWriter.write(value)
    bufferedWriter.newLine()
  }

  /**
    *
    * @param source
    */
  def openAndWrite(source: Source) = {
    val (lu, li, ag) = {
      (getBufferedWriter("lookup_user.csv"),
        getBufferedWriter("lookup_product.csv"),
        getBufferedWriter("agg_ratings.csv"))
    }
    transform(source) foreach { case (u, i, r) =>
      write(u._1 + "," + u._2,lu)
      write(i._1 + "," + i._2,li)
      write(u._2 + "," + i._2 + "," + r,ag)
    }
    lu.close()
    li.close()
    ag.close()
  }

  /**
    *
    * @param source
    * @return
    */
  protected def transform(source: Source) = {
    val (i1, i2) = parserCsv(source).duplicate
    val max = i1.map(_._4).max
    var cpt = 0
    i2.map { case (uid, iid, rating, timestamp) =>
      val u = (uid, cpt)
      val i = (iid, cpt+1)
      cpt += 2
      (u, i, computeRating(max, timestamp, rating))
    }.filter(_._3 > 0.01)
  }


}


object Main {

  import XagParser._

  def main(args: Array[String]) {
    val started = System.currentTimeMillis()
    Try {
      val file = new File(args(0))
      openAndWrite(Source.fromFile(file))
    }.toEither match {
      case Left(x) => throw new Exception("File on arg doesn't exist or path is not valid")
      case Right(_) => {
        val seconds = (System.currentTimeMillis() - started) / 1000
        println(s"Parser finished successfully, it took $seconds seconds")
      }
    }
  }

}
