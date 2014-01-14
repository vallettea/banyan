package net.snips.banyan.examples

import net.snips.banyan.{Utils, ForestGrower}
import net.snips.banyan.dataContainers.{FeatureType, FeatureValue}

import scala.collection.mutable.MutableList

object Cars extends App {

    val filename = "data/examples/cars.csv"

    val timer = new Utils.SimpleTimer()
    println("=================================")
    println("========== Car example ==========")
    // original dataset:
    // http://archive.ics.uci.edu/ml/datasets/Automobile

    // prepare data
    val selectedFeatures = scala.collection.SortedMap(
        "normalized-losses" -> Set("continuous"),
        "make" -> Set(), // if nothing provided, the categies will be determined
        "fuel-type" -> Set("diesel", "gas"),
        "aspiration" -> Set("std", "turbo"),
        "num-of-doors" -> Set("four", "two"),
        "body-style" -> Set("hardtop", "wagon", "sedan", "hatchback", "convertible"),
        "drive-wheels" -> Set("4wd", "fwd", "rwd"),
        "engine-location" -> Set("front", "rear"),
        "wheel-base" -> Set("continuous"),
        "length" -> Set("continuous"),
        "width" -> Set("continuous"),
        "height" -> Set("continuous"),
        "curb-weight" -> Set("continuous"),
        "engine-type" -> Set("dohc", "dohcv", "l", "ohc", "ohcf", "ohcv", "rotor"),
        "num-of-cylinders" -> Set("eight", "five", "four", "six", "three", "twelve", "two"),
        "engine-size" -> Set("continuous"),
        "fuel-system" -> Set("1bbl", "2bbl", "4bbl", "idi", "mfi", "mpfi", "spdi", "spfi"),
        "bore" -> Set("continuous"),
        "stroke" -> Set("continuous"),
        "compression-ratio" -> Set("continuous"),
        "horsepower" -> Set("continuous" ),
        "peak-rpm" -> Set("continuous"),
        "city-mpg" -> Set("continuous"),
        "highway-mpg" -> Set("continuous")
    )

    val (data, featureTypes) = Utils.DataPointsFromCsv(filename, "price", selectedFeatures)
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

