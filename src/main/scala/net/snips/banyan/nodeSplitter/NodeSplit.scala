package net.snips.banyan.nodeSplitter

import net.snips.banyan.dataContainers.{FeatureType, Point}

import scala.collection.mutable.{ArrayBuffer => MutableArrayBuffer}

case class NodeSplit(

    featureTypes: Array[FeatureType]

) {
    // Accumulate statistics regarding a point of data.
    def process(point: Point): Unit = {
        data.append(point)
    }

    // Evaluate all splitting features for a node and select the one with
    // the smallest error.
    def findBestSplit(): BestSplit = {
        var bestSplit: BestSplit = null
        for (featureIndex <- 0 until featureTypes.length) {
            val featureType = featureTypes(featureIndex)
            var currentBestSplit: BestSplit = null
            if (featureType.isOrdered) {
                currentBestSplit = orderedSplit.findBestSplit(featureIndex, data)
            } else currentBestSplit = categoricalSplit.findBestSplit(featureIndex, data)
            if (bestSplit == null || currentBestSplit.error < bestSplit.error) {
                bestSplit = currentBestSplit
            }
        }
        bestSplit
    }

    private val orderedSplit: NodeSplitBase = new OrderedNodeSplit
    private val categoricalSplit: NodeSplitBase = new CategoricalNodeSplit
    private val data = new MutableArrayBuffer[Point]
}





