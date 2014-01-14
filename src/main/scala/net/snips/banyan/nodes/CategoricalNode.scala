package net.snips.banyan.nodes

import net.snips.banyan.dataContainers.FeatureValue

import scala.collection.mutable.{ArrayBuffer => MutableArrayBuffer}


// A node for categorical data, e.g. for values in (1, 2, 3, 4)
// id: The id for the node
// featureIndex: The index into the array of FeatureValue that this splits.
// prediction: The prediction value, provided this is a leaf node.
class CategoricalNode(override val id: Int,
    override val featureIndex: Int,
    prediction: Double) extends Node(prediction) {

    // Return the child leaf node corresponding to the array of values.
    // All features are assumed present. If not enough features are provided
    // then return an EmptyNode.
    override def getLeaf(features: Array[FeatureValue]): Node = {
        if (isLeaf) this
        else if (features.length <= featureIndex) EmptyNode.getEmptyNode
        else if (categories.contains(features(featureIndex))) leftChild.getLeaf(features)
        else rightChild.getLeaf(features)
    }

    // Insert both left and right children and set the splitting values.
    override def insertChildren(newLeftChild: Node,
        newRightChild: Node,
        values: MutableArrayBuffer[FeatureValue]): Unit = {
        leftChild = newLeftChild
        rightChild = newRightChild
        categories = values.toSet[FeatureValue]  
    }

    override def splitString: String = categories.toString

    private var categories: Set[FeatureValue] = null
}
