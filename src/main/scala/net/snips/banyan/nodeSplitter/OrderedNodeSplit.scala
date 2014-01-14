package net.snips.banyan.nodeSplitter

import net.snips.banyan.dataContainers.{FeatureValue, Point, FeatureValueAndData}

import scala.collection.mutable.{ArrayBuffer => MutableArrayBuffer}


class OrderedNodeSplit extends NodeSplitBase {

    override def findBestSplit(featureIndex: Int, data: MutableArrayBuffer[Point]): BestSplit = {
        if (hasTooFewPoints(data.size)) return BestSplit.noSolution
        // TODO: replace with an in-place sort
        val sortedData = data.sortBy(_.features(featureIndex))

        // To determine the best
        // region split, we accumulate the individual components of sortedData.
        // The first element of accumulatedData is garbage. The real index starts
        // at 1.
        val accumulatedData = sortedData.scanLeft(
            new FeatureValueAndData(sortedData(0).features(featureIndex), 0, 0, 0)
            )(_.accumulate(featureIndex)(_))

        sortedData.clear()
        val predictionsAndError = for (index <- 1 until accumulatedData.size)
            yield PredictionAndError.compute(index, accumulatedData)
        val bestPredictionAndError = minError(accumulatedData, predictionsAndError)
        val leftCount = bestPredictionAndError.index
        val rightCount = accumulatedData.size - leftCount - 1
        // The 0 element is garbage.
        val splitData = accumulatedData.dropRight(accumulatedData.size - bestPredictionAndError.index - 1)
        val splitFeatures = new MutableArrayBuffer[FeatureValue]
        splitFeatures += accumulatedData(bestPredictionAndError.index).featureValue
        new BestSplit(
            featureIndex,
            splitFeatures,
            bestPredictionAndError.leftPrediction,
            bestPredictionAndError.rightPrediction,
            bestPredictionAndError.leftError,
            bestPredictionAndError.rightError,
            leftCount,
            rightCount
        )
    }

    private class FeatureData(val featureValue: FeatureValue, val yValue: Double) {}
}