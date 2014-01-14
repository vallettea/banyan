package net.snips.banyan.nodeSplitter

import net.snips.banyan.dataContainers.FeatureValue

import scala.collection.mutable.{ArrayBuffer => MutableArrayBuffer}

// The best splitting of either an ordered or categorical node,
// including the predictions for the left and right branches.
case class BestSplit(

    featureIndex: Int, // the feature concerned

    splitFeatures: MutableArrayBuffer[FeatureValue], // value at the split

    leftPrediction: Double, // average of values left of the split

    rightPrediction: Double, // average of values right of the split

    leftError: Double, 

    rightError: Double,

    leftCount: Int, // number of points left of the split 

    rightCount: Int 

) extends Ordered[BestSplit] {

    override def compare(other: BestSplit): Int = error.compare(other.error)
  
    def error: Double = leftError + rightError

    def isNotASolution: Boolean = {
        leftError + rightError == Double.PositiveInfinity
    }

    override def toString(): String = "(%d, %s, %f, %f, %f, %f, %d, %d)".format(
        featureIndex, splitFeatures.toString(), leftPrediction, rightPrediction,
        leftError, rightError, leftCount, rightCount)
}

object BestSplit {
    def noSolution: BestSplit = new BestSplit(-1, MutableArrayBuffer(),
        0.0, 0.0, Double.PositiveInfinity, Double.PositiveInfinity, 0, 0)
}
