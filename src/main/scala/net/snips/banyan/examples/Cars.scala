package net.snips.banyan.examples

import net.snips.banyan.{Utils, ForestGrower, Tree, TreeGrower}
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
    val cut = (6 * data.length/10.0).toInt
    val samplesLearn = data.take(cut)
    val samplesTest = data.takeRight(data.length - cut)

    println("==========================")
    println("Start learning with a tree")
    timer.start()

    val tree = new Tree(1.0, 500)
    val treeGrower = new TreeGrower(tree, featureTypes, samplesLearn)
    treeGrower.grow()
    println(s"Model fitted in ${timer.tick} ms")



    println("------- feature importance -------")
    val names = selectedFeatures.keys.toArray
    tree.featureImportance.toList.sortBy(_._2).reverse.foreach(f => 
        println(f"${names(f._1)}%18s => ${f._2 * 100}%2.2f ")
    )
    println("----------------------------------")
    println

    // check
    var expected = MutableList[Double]()
    var predicted = MutableList[Double]()
    samplesTest.foreach{ point => 
        expected += point.yValue
        predicted +=  tree.getPrediction(point.features)
        // println(s"${point.yValue}, ${forest.getPrediction(point.features)}")
    }
    
    println("Pearson: " + Utils.pearson(expected.toArray, predicted.toArray).toString)
    println("RSquared: " + Utils.RSquared(expected.toArray, predicted.toArray).toString)
    println("Rmse: " + Utils.rmse(expected.toArray, predicted.toArray).toString)




    println("==========================")
    println("Start learning with a forest")
    val forest = new ForestGrower(100, // MAXTREES 
                    15, //max node per tree
                    featureTypes,
                    samplesLearn).grow()

    println(s"Model fitted in ${timer.tick} ms")


    println("------- feature importance -------")
    forest.featureImportance.toList.sortBy(_._2).reverse.foreach(f => 
        println(f"${names(f._1)}%18s => ${f._2 * 100}%2.2f ")
    )
    println("----------------------------------")
    println

    // check
    expected.clear
    predicted.clear
    samplesTest.foreach{ point => 
        expected += point.yValue
        predicted +=  forest.getPrediction(point.features)
        // println(s"${point.yValue}, ${forest.getPrediction(point.features)}")
    }
    
    println("Pearson: " + Utils.pearson(expected.toArray, predicted.toArray).toString)
    println("RSquared: " + Utils.RSquared(expected.toArray, predicted.toArray).toString)
    println("Rmse: " + Utils.rmse(expected.toArray, predicted.toArray).toString)

}

