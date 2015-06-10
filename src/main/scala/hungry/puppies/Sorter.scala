package hungry.puppies

import scala.io.StdIn._


    object Sorter extends App {

      val input = readLine().split(" ").toVector.map(_.toInt)
      val t = System.currentTimeMillis
      val result = input.permutations.toParArray
        .map(vec => calcHappiness(vec))
        .maxBy(_._2)

      println(result._2)
      println(result._1.mkString(" "))
      println((System.currentTimeMillis - t) * 0.001)

      def calcHappiness(vector: Vector[Int]) = {
        (vector, vector.view.zipWithIndex.foldLeft(0) {
          case (acc, (value, index)) =>
            if (index == 0)
              acc + math.signum(value - vector(index+1))
            else if (index == vector.length - 1)
              acc + math.signum(value - vector(index-1))
            else acc + (math.signum(value - vector(index+1)) + math.signum(value - vector(index-1)))/2
        })
      }
    }
