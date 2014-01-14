package net.snips.banyan.nodeSplitter

import net.snips.banyan.dataContainers._

import org.scalatest._
import scala.collection.immutable.Set
import scala.collection.mutable.{ArrayBuffer => MutableArrayBuffer}
import scala.collection.mutable.{HashMap => MutableHashMap}

class NodeSplitTest extends FlatSpec with Matchers {

    "FeatureValueAndData" should "be equal" in {
        assert(new FeatureValueAndData(new FeatureValue(0), 1, 2.0, 3.0) ===
          new FeatureValueAndData(new FeatureValue(0), 1, 2.0, 3.0))
    }

    "FeatureValueAndData and Accumulate" should "have the same type" in {
        val data = Array(
            new FeatureValueAndData(new FeatureValue(0), 1, 2.0, 4.0),
            new FeatureValueAndData(new FeatureValue(3), 2, 1.0, 1.0),
            new FeatureValueAndData(new FeatureValue(6), 4, 3.0, 9.0),
            new FeatureValueAndData(new FeatureValue(7), 3, 6.0, 36.0),
            new FeatureValueAndData(new FeatureValue(9), 5, 4.0, 16.0))
        val accumulatedData = data.scanLeft(new FeatureValueAndData(
            data(0).featureValue, 0, 0, 0))(_ accumulate _)
        val expectedData = Array(
            new FeatureValueAndData(new FeatureValue(0), 0, 0.0, 0.0),
            new FeatureValueAndData(new FeatureValue(0), 1, 2.0, 4.0),
            new FeatureValueAndData(new FeatureValue(3), 3, 3.0, 5.0),
            new FeatureValueAndData(new FeatureValue(6), 7, 6.0, 14.0),
            new FeatureValueAndData(new FeatureValue(7), 10, 12.0, 50.0),
            new FeatureValueAndData(new FeatureValue(9), 15, 16.0, 66.0)) 
        assert(expectedData === accumulatedData)
    }

    "FeatureValueData "should "be equal" in {
        assert(new FeatureValueData(1, 2, 3) === new FeatureValueData(1, 2, 3))
    }

    "FeatureValueData " should "accumulate Y" in {
        val featureAndDataMap = new MutableHashMap[FeatureValue, FeatureValueData].
          withDefaultValue(new FeatureValueData(0, 0, 0))
        val points = Array(
            makePoint(1, 2),
            makePoint(3, 5),
            makePoint(1, 6),
            makePoint(4, 8),
            makePoint(4, 7))        
        for (point <- points) {
            val featureValue = point.features(0)
            featureAndDataMap.put(featureValue, featureAndDataMap(featureValue).accumulate(point.yValue))
        }
        val expectedMap = Map(
            new FeatureValue(1) -> new FeatureValueData(2, 8, 40),
            new FeatureValue(3) -> new FeatureValueData(1, 5, 25),
            new FeatureValue(4) -> new FeatureValueData(2, 15, 113)
        )
        assert(expectedMap === featureAndDataMap)
    }

    def makePoint(x: Int, y: Double): Point = {
        new Point(Array(new FeatureValue(x)), y)
    }

    def checkBestSplit(expectedBestSplit: BestSplit, actualBestSplit: BestSplit): Unit = {
        expectedBestSplit.featureIndex should be (actualBestSplit.featureIndex)
        expectedBestSplit.splitFeatures should be (actualBestSplit.splitFeatures)    
        expectedBestSplit.leftPrediction should be (actualBestSplit.leftPrediction)
        expectedBestSplit.rightPrediction should be (actualBestSplit.rightPrediction)
        expectedBestSplit.leftError should be (actualBestSplit.leftError)
        expectedBestSplit.rightError should be (actualBestSplit.rightError)
        expectedBestSplit.leftCount should be (actualBestSplit.leftCount)
        expectedBestSplit.rightCount should be (actualBestSplit.rightCount)
    }

    "Ordered Node Split" should "split at 3" in {
    val featuresType = Array(new FeatureType(true, Set(new FeatureValue(0))))
    val nodeSplit = new NodeSplit(featuresType)
    val points = Array(
        makePoint(5, 1),
        makePoint(1, -1),
        makePoint(3, -1),
        makePoint(10, 0),
        makePoint(7, -1),
        makePoint(9, -1)
    )
    for (point <- points) nodeSplit.process(point)
        val bestSplit = nodeSplit.findBestSplit()
        val expectedBestSplit = new BestSplit(0,
            new MutableArrayBuffer[FeatureValue] += new FeatureValue(3),
            -1.0, -0.25, 0.0, 2.75, 2, 4)
        checkBestSplit(expectedBestSplit, bestSplit)
    }

    "Ordered Node" should "split 2" in {
        val featuresType = Array(new FeatureType(true, Set(new FeatureValue(0))))
        val nodeSplit = new NodeSplit(featuresType)
        val points = Array(
            makePoint(1, 1),
            makePoint(3, 5),
            makePoint(5, 7),
            makePoint(7, 9),
            makePoint(9, 11))
        for (point <- points) nodeSplit.process(point)
        val bestSplit = nodeSplit.findBestSplit()
        val expectedBestSplit = new BestSplit(0,
            new MutableArrayBuffer[FeatureValue] += new FeatureValue(3),
            3.0, 9.0, 8.0, 8.0, 2, 3)
        checkBestSplit(expectedBestSplit, bestSplit)
    }

    "Categorical Node" should "split 1" in {
        val featuresType = Array(new FeatureType(false,
            Set(new FeatureValue(1),
                new FeatureValue(3),
                new FeatureValue(11))))
        val nodeSplit = new NodeSplit(featuresType)
        val points = Array(
            makePoint(1, 1),
            makePoint(3, 5),
            makePoint(5, 7),
            makePoint(7, 9),
            makePoint(9, 11))
        for (point <- points) nodeSplit.process(point)
        val bestSplit = nodeSplit.findBestSplit()
        val expectedBestSplit = new BestSplit(0,
            new MutableArrayBuffer[FeatureValue] += new FeatureValue(1) +=
              new FeatureValue(3),
            3.0, 9.0, 8.0, 8.0, 2, 3)
        checkBestSplit(expectedBestSplit, bestSplit)
    }

    "Split of 1 Point" should "have no Solution" in {
        val featuresType = Array(new FeatureType(true, Set(new FeatureValue(0))))
        val nodeSplit = new NodeSplit(featuresType)
        val points = Array(
            makePoint(5, 1)
            )
        for (point <- points) nodeSplit.process(point)
        val bestSplit = nodeSplit.findBestSplit()
        checkBestSplit(BestSplit.noSolution, bestSplit)
    }

    "Split of 2 points " should "have no Error" in {
        val featuresType = Array(new FeatureType(true, Set(new FeatureValue(0))))
        val nodeSplit = new NodeSplit(featuresType)
        val points = Array(
            makePoint(1, 1),
            makePoint(3, 5))
        for (point <- points) nodeSplit.process(point)
        val bestSplit = nodeSplit.findBestSplit()
        val expectedBestSplit = new BestSplit(0,
            new MutableArrayBuffer[FeatureValue] += new FeatureValue(1),
            1.0, 5.0, 0, 0, 1, 1)
        checkBestSplit(expectedBestSplit, bestSplit)
    }

    "Split of 3 points " should "have an error 1 side" in {
        val featuresType = Array(new FeatureType(true, Set(new FeatureValue(0))))
        val nodeSplit = new NodeSplit(featuresType)
        val points = Array(
            makePoint(3, 5),
            makePoint(2, 4),
            makePoint(1, 1))
        for (point <- points) nodeSplit.process(point)
        val bestSplit = nodeSplit.findBestSplit()
        val expectedBestSplit = new BestSplit(0,
            new MutableArrayBuffer[FeatureValue] += new FeatureValue(1),
            1.0, 4.5, 0, 0.5, 1, 2)
        checkBestSplit(expectedBestSplit, bestSplit)
    }

    class NodeError extends NodeSplitBase {
        override def findBestSplit(featureIndex: Int,
            data: MutableArrayBuffer[Point]): BestSplit = null

        def makeFVD(featureValue: Int, featureCount: Int,
            yValSum: Double, ySqValSum: Double): FeatureValueAndData = {
          new FeatureValueAndData(new FeatureValue(featureValue), featureCount,
              yValSum, ySqValSum)
        }

        def makePAE(index: Int, leftError: Double,
            rightError: Double): PredictionAndError = {
          new PredictionAndError(index, 0, 0, leftError, rightError)
        }

        val accumulatedData = MutableArrayBuffer(
            makeFVD(0, 0, 0, 0),
            makeFVD(1, 0, 0, 0),
            makeFVD(2, 0, 0, 0),
            makeFVD(2, 0, 0, 0),
            makeFVD(2, 0, 0, 0),
            makeFVD(3, 0, 0, 0),
            makeFVD(4, 0, 0, 0))
            
        val predictionAndError = Array(
            makePAE(1, 1, 1),
            makePAE(2, 0, 0.2),
            makePAE(2, 0, 0),
            makePAE(2, 0.2, 0),
            makePAE(3, 0.5, 0.5),
            makePAE(4, 0.5, 0.5)
            )

        def getMinError = minError(accumulatedData, predictionAndError.toIndexedSeq)
    }

    "Min Error" should "work" in {
        val nodeError = new NodeError
        val predictionAndError = nodeError.getMinError
        predictionAndError.leftError should be (0.2)
        predictionAndError.rightError should be (0.0)
    }

}