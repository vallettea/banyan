package net.snips.banyan

import net.snips.banyan.dataContainers.FeatureValue

import scala.collection.mutable.ArrayBuffer

// A forest is a collection of trees used in prediction.
class Forest {
  
    // Return the prediction for a group of features.
    def getPrediction(features : Array[FeatureValue]): Double = {
        trees.map(tree => tree.getPrediction(features)).sum
    }

    // Return the feature importance of the forest
	def featureImportance: Map[Int, Double] = {

		val nbTrees = trees.size

		def mergeMap(map1: Map[Int, Double], map2: Map[Int, Double]): Map[Int, Double] = {
			(map1.keySet ++ map2.keySet).map(key => (key, map1.getOrElse(key, 0.0d) + map2.getOrElse(key, 0.0d))).toMap
		}

		if (nbTrees > 1) {
			trees.map(tree => tree.featureImportance)
				 .filter(m => !(m.values.toSet contains Double.NaN))
				 .reduce(mergeMap(_, _))
				 .map(kv => (kv._1, kv._2/nbTrees))
		} else Map[Int, Double]()
	}

    def addTree(tree: Tree): Unit = trees.append(tree)

    private val trees = new ArrayBuffer[Tree]
}