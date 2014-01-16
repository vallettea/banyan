package net.snips.banyan.examples

import net.snips.banyan.{Utils, ForestGrower, Tree, TreeGrower}
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

