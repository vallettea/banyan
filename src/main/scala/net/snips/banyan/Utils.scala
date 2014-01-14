package net.snips.banyan

import net.snips.banyan.dataContainers.{FeatureValue, FeatureType, Point}

import scala.collection.mutable.{Set => MutableSet}
import scala.io.Source
import scala.util.{Try, Failure, Success}

object Utils {

    def DataPointsFromCsv(filename: String, toPredict: String, selectedFeatures: scala.collection.SortedMap[String, Set[_ <: String]]): (Array[Point], Array[FeatureType]) = {

        println("Loading data:")
        val lines = Source.fromFile(filename).getLines
        val headers: Map[Int, String] = lines.next.split(",").zipWithIndex.map(x => x._2 -> x._1).toMap

        var possibleValues = headers.values.map(feature => feature -> MutableSet[Any]()).toMap
        var errors = 0
        var i = 0

        val points = lines.flatMap(l => {
            Try {
                if(i % 1000 == 0) print(i + " lines                 \r")
                i += 1
                if(i % 100000 == 0) System.gc
                
                val ss = l.trim.split(",").zipWithIndex.map(x => headers(x._2) -> x._1.trim).toMap
                val features = selectedFeatures.keys.toArray.map(k => new FeatureValue(ss(k)))
                val yValue = ss(toPredict).toDouble
                selectedFeatures.keys.toArray.foreach{ feature => 
                    possibleValues(feature) += ss(feature)
                }
                Some(new Point(features, yValue))
            } match {
                case Success(x) => x
                case Failure(x) => errors += 1; None
            }
        }).toArray

        println(s"Errors during parsing: ${errors}")
        // shuffle points
        val data = util.Random.shuffle(points.toList).toArray

        // featureTypes
        val featureTypes = selectedFeatures.map(f => {
            // case the feature is ordered 
            if (f._2 == Set("continuous")) {
                new FeatureType(true, Set[FeatureValue]())
            }
            // case the feature is categorical and no value is provided
            else if (f._2 == Set()) {
                new FeatureType(false, possibleValues(f._1).map(v => new FeatureValue(v)).toSet)
            }
            // case the feature is categorical and a set of permissible value si provided
            else {
                new FeatureType(false, f._2.map(v => new FeatureValue(v)))
            }
        }).toArray

        (data, featureTypes)

    }


    // Training gradient boosted trees requires fitting a tree to the input
    // data where y-Values are replaced by the negative derivative of a
    // loss function evaluated at the training residual. This class turns
    // an existing PointIterator into one which provides said modification
    // to the training data.
    //
    // trainingData: Array of training data.
    // lossFunction:  A loss function, e.g. HuberLoss, SquaredLoss.
    // forest:        The forest that we are currently training / growing.
    def DifferentialData(trainingData: Array[Point], lossFunction: LossFunction, forest: Forest): Array[Point] = {

        trainingData.map( point => {
            val prediction: Double = forest.getPrediction(point.features)
            val newYVal = lossFunction.getNegDerivative(point.yValue - prediction)
            new Point(point.features, newYVal)
        })
      
    }


    def pearson(expected: Array[Double], predicted: Array[Double]): Double = {

        val N = predicted.length

        val x2 = (predicted, predicted).zipped.map(_ * _).sum
        val y2 = (expected, expected).zipped.map(_ * _).sum
        val xy = (predicted, expected).zipped.map(_ * _).sum

        val avgX = predicted.sum/predicted.length
        val avgY = expected.sum/expected.length

        val stdX = math.sqrt(x2/N - avgX * avgX)
        val stdY = math.sqrt(y2/N - avgY * avgY)


        if (stdX != 0 & stdY != 0 ) {
            (xy - N * avgX * avgY) / (N * stdX * stdY)
        } else {
            0.0d
        }        

    }

    def RSquared(expected: Array[Double], predicted: Array[Double]): Double = {

        val N = predicted.length

        val x2 = (predicted, predicted).zipped.map(_ * _).sum
        val y2 = (expected, expected).zipped.map(_ * _).sum
        val xy = (predicted, expected).zipped.map(_ * _).sum

        val sumY = expected.sum
        val avgY = expected.sum/expected.length

        val sstot = y2 + N * avgY * avgY - 2 * avgY * sumY
        val ssres = y2 + x2 - 2 * xy

        1 - (ssres / sstot)

    }


    def rmse(expected: Array[Double], predicted: Array[Double]): Double = {

        val N = predicted.length

        val x2 = (predicted, predicted).zipped.map(_ * _).sum
        val y2 = (expected, expected).zipped.map(_ * _).sum
        val xy = (predicted, expected).zipped.map(_ * _).sum

        math.sqrt((x2 + y2 - 2 * xy) / N)

    }


    class SimpleTimer {

        var timestamp = System.currentTimeMillis
        var dtimestamp = System.currentTimeMillis

        def start() {
            timestamp = System.currentTimeMillis
            dtimestamp = System.currentTimeMillis
        }

        def tick(): Long = {
            val past = dtimestamp
            dtimestamp = System.currentTimeMillis
            dtimestamp - past
        }

        def total(): Long = {
            System.currentTimeMillis - timestamp
        }

    }

}
