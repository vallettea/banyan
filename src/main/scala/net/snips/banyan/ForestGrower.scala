package net.snips.banyan

import net.snips.banyan.dataContainers.{FeatureType, Point}

import scala.collection.mutable.HashMap

// A class for growing a forest of trees based on gradient boosting.
//
// maxTrees:        The number of trees to train.
// maxNodesPerTree: The maximum number of nodes per tree.
// featureTypes:    An array of the types of each feature, e.g. an array
//                  indicating whether the feature at a particular index
//                  is either an ordered or categorical feature.
// trainingData:   The training data.

case class ForestGrower(
    maxTrees: Int,
    maxNodesPerTree: Int,
    featureTypes: Array[FeatureType],
    trainingData: Array[Point]
) {

    def grow(): Forest = {
        val forest: Forest = new Forest
        createInitialTree(featureTypes, trainingData, forest)

        var j = 0

        for (i <- 1 until maxTrees) {

        if((100 * j / maxTrees.toFloat).toInt % 1 == 0) print((100 * j / maxTrees.toFloat).toInt + " %                 \r")
        j += 1
        val newTree = new Tree(1.0, maxNodesPerTree)
        // TODO: Make the loss function selectable.
        val lossFunction = new SquaredLoss
        val differentialData = Utils.DifferentialData(trainingData, lossFunction, forest)
        val treeGrower = new TreeGrower(newTree, featureTypes, differentialData)
        treeGrower.grow()
        forest.addTree(newTree)

      }
    forest
  }
  
    private def createInitialTree(featureTypes: Array[FeatureType],
        trainingData: Array[Point], forest: Forest): Unit = {
        // A single node tree will train to the average y-value
        val newTree = new Tree(1.0, 1)
        val treeGrower = new TreeGrower(newTree, featureTypes, trainingData)
        treeGrower.grow()
        forest.addTree(newTree)
    }

}