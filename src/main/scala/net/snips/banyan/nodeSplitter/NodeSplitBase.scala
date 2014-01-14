package net.snips.banyan.nodeSplitter

import net.snips.banyan.dataContainers.{FeatureValue, Point, FeatureValueAndData}

import scala.collection.immutable.IndexedSeq
import scala.collection.mutable.{ArrayBuffer => MutableArrayBuffer}


// A class used to find the best split for a node.
abstract class NodeSplitBase {
    // For a split based on a feature at a given index, find
    // the splitting which minimizes the squared error.
    def findBestSplit(featureIndex: Int, data: MutableArrayBuffer[Point]): BestSplit

    // A helper class to contain the prediction index, and the prediction
    // values over the left and right regions along with the prediction error.
    // This class is a bit redundant with BestSplit, but consumes slightly
    // less memory.
    protected class PredictionAndError(
        val index: Int,
        val leftPrediction: Double,
        val rightPrediction: Double,
        val leftError: Double,
        val rightError: Double
    ) extends Ordered[PredictionAndError] {
    
        def error: Double = leftError + rightError

        override def compare(other: PredictionAndError) = error.compare(other.error)

        override def toString: String = "%d, %f, %f, %f, %f".format(index,
            leftPrediction, rightPrediction, leftError, rightError)
    }

    protected object PredictionAndError {
        // Compute the predictions for the left and right regions and the
        // prediction error based on a data split at an index.
        // The error is Sum (y_i - prediction)^2 over region R_i
        // The solution is prediction = average y_i over region R_i, with 
        // error = Sum y_i ^2 - 2 * prediction * Sum y_i + N * prediction^2
        // where N is the number of points in region R_i. 
        def compute(
            index: Int,
            accumulatedData: MutableArrayBuffer[FeatureValueAndData]
            ):PredictionAndError = {
            val first = accumulatedData(index)
            val last = accumulatedData.last
            // The average y-value over the left partition.
            val leftPrediction = first.yValSum / first.featureCount
            // The average y-value over the right partition.
            var rightPrediction: Double = 0.0
            // accumulatedData.length = #points + 1. To avoid a NaN for the rightPrediction,
            // we must explicitly set it to 0 for a split of 1 point.
            if (accumulatedData.length > 2) {
                rightPrediction = (last.yValSum - first.yValSum) / (last.featureCount - first.featureCount)
            }
            def partitionError(
                prediction: Double,
                first: FeatureValueAndData,
                last: FeatureValueAndData) = {
                last.ySqValSum - first.ySqValSum +
                - 2 * prediction * (last.yValSum - first.yValSum) +
                (last.featureCount - first.featureCount) * prediction * prediction
            }
            val leftError =
                partitionError(leftPrediction,
                    new FeatureValueAndData(new FeatureValue(0), 0, 0, 0),
                    first)
            val rightError = partitionError(rightPrediction, first, last)
            new PredictionAndError(index,
              leftPrediction,
              rightPrediction,
              leftError,
              rightError)
        }

        def notASolution: PredictionAndError = new PredictionAndError(0, 0, 0,
            Double.PositiveInfinity, Double.PositiveInfinity)
    }
  
    protected def minError(accumulatedData: MutableArrayBuffer[FeatureValueAndData],
        predictionAndError: IndexedSeq[PredictionAndError]):
        PredictionAndError = {
        var bestPredictionAndError: PredictionAndError = PredictionAndError.notASolution
        // TODO: consider generalizing the node splitting as a protection
        // against noisy data, e.g. probably better not to split
        // the data so that 1 training point goes to the left branch while
        // 1000 training points go to the right.
        for(i <- 0 until predictionAndError.length - 1) {
            if (accumulatedData(i + 1).featureValue !=
                accumulatedData(i + 2).featureValue &&
                  predictionAndError(i) < bestPredictionAndError) {
                bestPredictionAndError = predictionAndError(i)
            }
        }
        if (bestPredictionAndError.error ==
            PredictionAndError.notASolution.error) {
            predictionAndError(0)
        } else bestPredictionAndError
    }

    // Do not continue with node split calculations if there are too few points.
    protected def hasTooFewPoints(size: Int): Boolean = size <= 1
}

