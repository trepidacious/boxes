package boxes.demo

/**
 * Created by IntelliJ IDEA.
 * User: trepidacious
 * Date: 13/06/2011
 * Time: 21:36
 * To change this template use File | Settings | File Templates.
 */

object RandomProto {
  def main(args: Array[String]) {
    println(List(1, 3, 9).scanLeft(0){(c, e) => c + e})
  }
}