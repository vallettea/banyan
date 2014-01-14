package net.snips.banyan

import net.snips.banyan.dataContainers.FeatureValue
import net.snips.banyan.nodes._

import scala.collection.mutable.{ArrayBuffer => MutableArrayBuffer}

// A classification or regression tree. The tree is trained by
// the class TreeGrower.

case class Tree(

  weight: Double, // The weight to associate with this tree in prediction.

  maxNodes: Int // The maximum number of nodes for this tree.

) {
  
    // Return the weighted prediction for this tree.
    def getPrediction(features: Array[FeatureValue]): Double = weight * root.getPrediction(features)

    // **************************************************************************
    //
    // Functions for growing the tree.
    //
    // **************************************************************************

    // Return the leaf node corresponding to a vector of features.
    def getLeaf(features: Array[FeatureValue]): Node = root.getLeaf(features)

    // Return all of the leaves for the tree. In the event of an empty tree,
    // an empty MutableArrayBuffer is returned.
    def getLeaves: MutableArrayBuffer[Node] = {
        val leaves = new MutableArrayBuffer[Node]
        def findLeaves(current: Node): Unit = {
            if (!current.isEmptyNode) {
                if (current.isLeaf) leaves.append(current)
                else {
                    findLeaves(current.getLeftChild)
                    findLeaves(current.getRightChild)
                }
            }
        }
        findLeaves(root)
        leaves
    }

    def insertChildren(parent: Node,
                      leftChild: Node,
                      rightChild: Node,
                      values: MutableArrayBuffer[FeatureValue]): Unit = {
        parent.insertChildren(leftChild, rightChild, values)
        nodeCount += 2
    }

    // Indicates whether the tree is fully grown or not.
    def isFull: Boolean = nodeCount >= maxNodes

    // Replace the root node. Do not increment the node count
    def replaceRootNode(node: Node): Unit = root = node

    // Set the root node. Increment the node count.
    def setRootNode(node: Node): Unit = {
        root = node
        nodeCount += 1
    }

    def size: Int = nodeCount

    override def toString: String  = root.toString

    // The number of nodes in the tree.
    private var nodeCount: Int = 0

    // The root node for the tree.
    private var root: Node = EmptyNode.getEmptyNode

}