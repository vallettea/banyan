package net.snips.banyan.nodes

import net.snips.banyan.dataContainers.FeatureValue


// A type of data node used for a leaf when growing a tree because
// the feature index has not yet been decided.
class UnstructuredNode(
    prediction: Double,
    val parent: Node,
    val error: Double
) extends Node(prediction) {
  
    override def getError: Double = error

    // Unstructured nodes can only be leaves.
    override def getLeaf(features: Array[FeatureValue]): Node = this

    // Replace the Unstructured node with another node, which will either
    // be an ordered or categorical node. Assumes that parent is
    // not an EmptyNode.
    override def replaceNode(newNode: Node): Unit = {
        if (this eq parent.getLeftChild) parent.setLeftChild(newNode)
        else if (this eq parent.getRightChild) parent.setRightChild(newNode)
    }

    override def splitString: String = parent.splitString
}