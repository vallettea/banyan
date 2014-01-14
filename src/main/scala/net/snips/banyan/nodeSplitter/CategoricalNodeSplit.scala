package net.snips.banyan.nodeSplitter

import net.snips.banyan.dataContainers.{FeatureValue, Point, FeatureValueAndData, FeatureValueData}

import scala.collection.mutable.{HashMap => MutableHashMap, ArrayBuffer => MutableArrayBuffer}


class CategoricalNodeSplit extends NodeSplitBase {
  
    override def findBestSplit(featureIndex: Int, data: MutableArrayBuffer[Point]): BestSplit = {
        if (hasTooFewPoints(data.size)) return BestSplit.noSolution
        // A map from a FeatureValue/x-value (Double, Int, or String) to data 
        // representing (count, y-value sum). We will use Breiman's algorithm
        // for determining an optimal split by examining splits ordered by increasing
        // average y-value.
        val featureAndDataMap = new MutableHashMap[FeatureValue, FeatureValueData].
            withDefaultValue(new FeatureValueData(0, 0, 0))
        for (point <- data) {
            val featureValue = point.features(featureIndex)
            featureAndDataMap.put(featureValue, featureAndDataMap(featureValue).accumulate(point.yValue))
        }
        // Convert to an MutableArrayBuffer of FeatureValueAndData.
        val combinedData = new MutableArrayBuffer[FeatureValueAndData]
        for ((featureValue, featureValueData) <- featureAndDataMap) { 
            combinedData += new FeatureValueAndData(featureValue,
                featureValueData.featureCount,
                featureValueData.yValSum,
                featureValueData.ySqValSum)
        }
        featureAndDataMap.clear()
        // TODO: replace with an in-place sort
        val sortedData = combinedData.sortBy({ x => x.yValSum / x.featureCount })
        combinedData.clear()
        
        // To determine the best
        // region split, we accumulate the individual components of sortedData.
        // The first element of accumulatedData is garbage. The real index starts
        // at 1.
        val accumulatedData = sortedData.scanLeft(new FeatureValueAndData(
            sortedData(0).featureValue, 0, 0, 0))(_ accumulate _)
        sortedData.clear()
        val predictionsAndError= for (index <- 1 until accumulatedData.size)
        yield PredictionAndError.compute(index, accumulatedData)
        val bestPredictionAndError = minError(accumulatedData, predictionsAndError)
        val leftCount = bestPredictionAndError.index
        val rightCount = accumulatedData.size - leftCount - 1
        // The 0 element is garbage.
        val splitData = accumulatedData.dropRight(
            accumulatedData.size - bestPredictionAndError.index - 1)
        val splitFeatures = new MutableArrayBuffer[FeatureValue]
        for (index <- 1 until splitData.size) {
          splitFeatures += splitData(index).featureValue
        }
        new BestSplit(featureIndex,
            splitFeatures,
            bestPredictionAndError.leftPrediction,
            bestPredictionAndError.rightPrediction,
            bestPredictionAndError.leftError,
            bestPredictionAndError.rightError,
            leftCount,
            rightCount)
        }
}