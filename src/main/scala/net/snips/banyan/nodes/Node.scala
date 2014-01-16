package net.snips.banyan.nodes

import net.snips.banyan.dataContainers.FeatureValue

import scala.collection.mutable.{ArrayBuffer => MutableArrayBuffer}
import scala.reflect.BeanProperty


abstract class Node {
    
    def this(pred: Double) {
        this()
        prediction = pred
    }
  
    // Node errors are currently only stored at the leaves.
    def getError: Double = Double.PositiveInfinity

    // Get the node id.
    def getId = id

    // Get the index of the feature that this node splits.
    def getFeatureIndex = featureIndex

    // Get the leaf node corresponding to an array of feature values.
    def getLeaf(features: Array[FeatureValue]): Node = EmptyNode.getEmptyNode

    // Get the prediction associated with this node, which may or may not be a
    // leaf node.
    def getNodePrediction: Double = prediction

    // Given the array of feature values, find the prediction for the corresponding
    // leaf node.
    def getPrediction(features : Array[FeatureValue]): Double = {
        if (isEmptyNode) 0.0
        else {
          val leaf = getLeaf(features)
          if (leaf.isEmptyNode) 0.0
          else leaf.getNodePrediction
        }
    }

    def isEmptyNode = false

    def isLeaf = leftChild.isEmptyNode && rightChild.isEmptyNode

    // Insert a left and right child at a node, which must be a leaf.
    // values: The splitting values. For an ordered node, this must
    //         have exactly one.
    def insertChildren(newLeftChild: Node,
      newRightChild: Node,
      values: MutableArrayBuffer[FeatureValue]): Unit = {
        leftChild = newLeftChild
        rightChild = newRightChild
    }

    // Replace this node with a new one. Must be used on leaves only.
    def replaceNode(newNode: Node): Unit = {}

    def setPrediction(pred: Double): Unit = prediction = pred


    protected val id: Int = -1
    protected val featureIndex: Int = -1
    @BeanProperty protected var leftChild: Node = EmptyNode.getEmptyNode
    @BeanProperty protected var rightChild: Node = EmptyNode.getEmptyNode
    protected var prediction: Double = 0.0

    // Return the splitting value as a string.
    def splitString: String = ""

    def nbSamples: Int = 0
    def impurity: Double = 0.0

    // Generate a string of the leaves with the prediction and splitting value(s)
    override def toString: String = { stringTraverse("") }

    // A helper function used by toString.
    private def stringTraverse(path: String): String = {        
        if (!isLeaf) {
            "(%s) \n (%s)".format(leftChild.stringTraverse(path + "L"), rightChild.stringTraverse(path + "R"))  
        } else {
            "%s: %s = %f @ %s\n".format(getId, path, prediction, splitString)
        }
    }

}

