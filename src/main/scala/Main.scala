import Monoid.{given _}

object Main {

  def main(args: Array[String]): Unit = {
    println("Hello world!")
    println(msg)

    // Monoid[Int].cat(2,3)
    ()
  }

  def msg = "I was compiled by dotty :)"

}
