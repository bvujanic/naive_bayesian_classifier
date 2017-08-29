import scala.io.Source
import scala.collection.mutable.ListBuffer
import scala.util.Random



object reader{

  def using[A <: { def close(): Unit }, B](resource: A)(f: A => B): B =
    try {
      f(resource)
    } finally {
      resource.close()
    }

  def csvParser(fileName: String): ListBuffer[List[Double]]= {

      var csvList: ListBuffer[List[Double]] = ListBuffer()

      reader.using(Source.fromFile(fileName)) { source => {

      for (line <- source.getLines()) {

        csvList += line.split(",").map(v => v.toDouble).toList
      }
    }
      return csvList;
    }
      null
  }
}

object split{

  def getSets(list: ListBuffer[List[Double]], splitRatio: Double): ListBuffer[ListBuffer[List[Double]]]={
      var trainSize: Int = (list.length * splitRatio).toInt
      val trainSet: ListBuffer[List[Double]] = ListBuffer()

      while(trainSize > 0){

          trainSize-=1
          var index: Int = Random.nextInt(list.length)
          trainSet += list(index)
          list.remove(index)

      }
      println(trainSet.length)
      println(list.length)

      ListBuffer(trainSet, list)

  }


  def splitByClasses(dataSet: ListBuffer[List[Double]]): Map[Double, ListBuffer[List[Double]]]  ={

    var setsByClass:Map[Double, ListBuffer[List[Double]]] = Map()
    for(elem <- dataSet){

      if(setsByClass.contains(elem.last)) {
        setsByClass(elem.last) += elem.dropRight(1)
      }
      else {
        setsByClass += elem.last -> ListBuffer(elem.dropRight(1))
      }
    }

        setsByClass

  }
}

object classifier {

  def mean(numbers: List[Double]): Double ={
    var sum: Double = 0

    for(number <- numbers){
      sum += number
    }

    sum/numbers.length

  }

  def stdev(numbers: List[Double]): Double ={
    val mean: Double = classifier.mean(numbers)
    var sum: Double = 0

    for(number <- numbers){
      sum += Math.pow(number - mean, 2)
    }

    Math.sqrt(sum/(numbers.length-1))
  }

  def transponseMatrix(matrix: ListBuffer[List[Double]]): ListBuffer[List[Double]]={
      var matrixT: ListBuffer[List[Double]] = ListBuffer()

      for( i <-  matrix.head.indices){
          var collumn: ListBuffer[Double] = new ListBuffer()
          for (j <- matrix.indices){
          collumn += matrix(j)(i)
          }
        matrixT += collumn.toList
      }

        matrixT
  }
  def summarize(dataSets: ListBuffer[List[Double]]): ListBuffer[List[Double]] ={


    var summed: ListBuffer[List[Double]] = ListBuffer()

    for(set <- transponseMatrix(dataSets)){
      summed += List(mean(set), stdev(set))
    }

    summed

  }


  def summarizeByClass( classified: Map[Double, ListBuffer[List[Double]]]): Map[Double, ListBuffer[List[Double]]] ={

        var classifiedSums: Map[Double, ListBuffer[List[Double]]] = Map()
         for (elem <- classified){
          classifiedSums += elem._1 -> summarize(elem._2)
         }
        classifiedSums
  }

  def calculateGaussianProbability(x: Double, mean: Double, stdev: Double): Double ={
    var exponent = Math.exp(-(Math.pow(x-mean,2)/(2*Math.pow(stdev,2))))
    1/(Math.sqrt(2*Math.PI) * stdev) * exponent
  }


  def calculateClassProbability(inputVector: List[Double], summary: Map[Double, ListBuffer[List[Double]]]): Map[Double, Double] ={

    var classifiedProbs: Map[Double, Double] = Map()
    for (elem <- summary){

      var calculatedProb: Double = 1.0
      var ctr: Int = 0

        for (sum <- elem._2) {
          calculatedProb *= calculateGaussianProbability(inputVector(ctr), sum.head, sum.last )
          ctr+=1
        }
      classifiedProbs += elem._1 ->  calculatedProb


    }

      classifiedProbs
  }

  def predict(inputVector: List[Double], summary: Map[Double, ListBuffer[List[Double]]]): Map[Double, Double]={
    var probability = calculateClassProbability(inputVector, summary)
    Map(probability.maxBy { case (key, value) => value })
  }

  def checkAccuracy(testSet: ListBuffer[List[Double]], classified: Map[Double, ListBuffer[List[Double]]]): Double={
    var tp: Int = 0
    for(testVector <- testSet)
    if(predict(testVector, classified).head._1 == testVector.last)
        tp += 1
    (100.0/testSet.length) * tp
  }
}



object Bayes {
  def main(args :Array[String]): Unit ={

    val file: String ="/home/bizzarec/Projects/DataScience/NaiveBayesianClassifier/iris.csv"
    var dataSet = reader.csvParser(file)
    var noElems: Int = dataSet.length
    var splitRatio = 0.80
    var sets = split.getSets(dataSet, splitRatio)
    var trainSet = sets.head
    var testSet = sets.last

    var trainSetByClass = split.splitByClasses(trainSet)
    var summedTrainingSet = classifier.summarizeByClass(trainSetByClass)


    println("Out of " + noElems + ", " + trainSet.length + " have been used for training, and " +  testSet.length + " were used for testing" )
    println("Naive bayesian clasiffier achieved accuracy of " + classifier.checkAccuracy(testSet, summedTrainingSet) + " true positives")

  }
}
