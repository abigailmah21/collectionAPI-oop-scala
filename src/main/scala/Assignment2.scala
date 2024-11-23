import java.io.FileReader
import java.io.File
import collection.JavaConverters._
import com.opencsv.CSVReader

object Assignment2 extends App {
  case class Data(product: String, daily_total: Int, D1_Headache: Int, D2_Headache:Int, D1_Vomiting: Int)

  val csvfile = getClass.getResource("aefi.csv").getPath
  val reader = new CSVReader(new FileReader(new File(csvfile)))
  val Vaccine:List[Array[String]] = reader.readAll().asScala.toList.tail;

  val vaccineDetails: Map[String, List[Data]] = Vaccine.map { row =>
    val product = row(1)
    val daily_total = row(2).toInt
    val D1_Headache = row(12).toInt
    val D2_Headache = row(24).toInt
    val D1_Vomiting = row(17).toInt
    val data = Data(product, daily_total, D1_Headache, D2_Headache, D1_Vomiting)
    (product, data)
  }.groupBy(_._1).mapValues(_.map(_._2))

  // Q1
  def commonUsedVaccine(dataMap: Map[String, List[Data]]): Unit = {
    val dailyTotal = dataMap.map {case (product, data) =>
      val total = data.map(_.daily_total).sum
      (product, total)
    }
    val result = dailyTotal.maxBy(_._2)
    println(s"The most commonly used vaccine product by Malaysian is ${result._1} with a total number of ${result._2}.")
  }

  // Q2
  def avgHeadache(dataMap: Map[String, List[Data]]): Unit = {
    dataMap.foreach { case(product, data) =>
      val totalHeadaches = data.map(data => data.D1_Headache + data.D2_Headache).sum
      val averageHeadaches = if (data.nonEmpty) totalHeadaches.toDouble / data.size else 0.0
      println(s"Average Occurrence of Headache for ${product} is ${averageHeadaches}.")
    }
  }

  // Q3
  def highestOccurrenceVomiting(dataMap:Map[String,List[Data]]):Unit = {
    val vomitingTotal = dataMap.map { case(product, data) =>
      val total= data.map(_.D1_Vomiting).sum
      (product,total)}
    val highestVomiting = vomitingTotal.maxBy(_._2)
    println(s"Vaccination type with the highest occurrence of vomiting after d1 is ${highestVomiting._1}, with a total of ${highestVomiting._2}.")
  }

  println("Question 1:")
  commonUsedVaccine(vaccineDetails)
  println("\nQuestion 2:")
  avgHeadache(vaccineDetails)
  println("\nQuestion 3")
  highestOccurrenceVomiting(vaccineDetails)
}


