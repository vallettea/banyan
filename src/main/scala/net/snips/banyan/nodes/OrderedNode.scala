package net.snips.banyan.nodes

import net.snips.banyan.dataContainers.FeatureValue

import scala.collection.mutable.{ArrayBuffer => MutableArrayBuffer}


// A node for ordered data, e.g. value <= 1 and value > 1
// id: The id for the node
// featureIndex: The index into the array of FeatureValue that this splits.
// prediction: The prediction value, provided this is a leaf node.
class OrderedNode(override val id: Int,
    override val featureIndex: Int,
    prediction: Double) extends Node(prediction) {

    // Return the child leaf node corresponding to the array of values.
    // All features are assumed present. If not enough features are provided
    // then return an EmptyNode.
    override def getLeaf(features: Array[FeatureValue]): Node = {
        if (isLeaf) this
        else if (features.length <= featureIndex) EmptyNode.getEmptyNode
        else if (features(featureIndex) <= splitValue) leftChild.getLeaf(features)
        else rightChild.getLeaf(features)
    }

    // Insert both left and right children and set the splitting value.
    override def insertChildren(newLeftChild: Node,
      newRightChild: Node,
      values: MutableArrayBuffer[FeatureValue]): Unit = {
        leftChild = newLeftChild
        rightChild = newRightChild
        splitValue = values(0)
    }

    override def splitString: String = splitValue.toString

    private var splitValue: FeatureValue = new FeatureValue(0)
}
