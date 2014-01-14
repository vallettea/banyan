package net.snips.banyan.examples

import net.snips.banyan.{Utils, ForestGrower}
import net.snips.banyan.dataContainers.{FeatureType, FeatureValue}
import scala.collection.mutable.MutableList

object Housing extends App {

    val filename = "data/examples/housing.csv"

    val timer = new Utils.SimpleTimer()
    println("=================================")
    println("======== Housing example =========")
    // original dataset:
    // http://archive.ics.uci.edu/ml/datasets/Housing

    // prepare data
    val selectedFeatures = scala.collection.SortedMap(
        "CRIM" -> Set("continuous"),
        "ZN" -> Set("continuous"),
        "INDUS" -> Set("continuous"),
        "CHAS" -> Set("continuous"),
        "NOX" -> Set("continuous"),
        "RM" -> Set("continuous"),
        "AGE" -> Set("continuous"),
        "DIS" -> Set("continuous"),
        "RAD" -> Set("continuous"),
        "TAX" -> Set("continuous"),
        "PTRATIO" -> Set("continuous"),
        "B" -> Set("continuous"),
        "LSTAT" -> Set("continuous")
    )
    val (data, featureTypes) = Utils.DataPointsFromCsv(filename, "MEDV", selectedFeatures)
    val cut = (9 * data.length/10.0).toInt
    val samplesLearn = data.take(cut)
    val samplesTest = data.takeRight(data.length - cut)

    println("Start learning")
    timer.start()
    val forest = new ForestGrower(100, // MAXTREES 
             5, //max node per tree
             featureTypes,
             samplesLearn).grow()

    println(s"Model fitted in ${timer.tick} ms")

    // check
    var expected = MutableList[Double]()
    var predicted = MutableList[Double]()
    samplesTest.foreach{ point => 
        expected += point.yValue
        predicted +=  forest.getPrediction(point.features)
        // println(s"${point.yValue}, ${forest.getPrediction(point.features)}")
    }
    println("Pearson: " + Utils.pearson(expected.toArray, predicted.toArray).toString)
    println("RSquared: " + Utils.RSquared(expected.toArray, predicted.toArray).toString)
    println("Rmse: " + Utils.rmse(expected.toArray, predicted.toArray).toString)

}

