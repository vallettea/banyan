package net.snips.banyan

import net.snips.banyan.dataContainers.FeatureValue
import net.snips.banyan.nodes._

import scala.collection.mutable.{ArrayBuffer => MutableArrayBuffer, Map => MutableMap}

// A classification or regression tree. The tree is trained by
// the class TreeGrower.

case class Tree(

    weight: Double, // The weight to associate with this tree in prediction.

    maxNodes: Int, // The maximum number of nodes for this tree.

    private var nodeCount: Int = 0,  // The number of nodes in the tree.

    private var root: Node = EmptyNode.getEmptyNode  // The root node for the tree.

) {
  
    // Return the weighted prediction for this tree.
    def getPrediction(features: Array[FeatureValue]): Double = weight * root.getPrediction(features)

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

    // Return the feature importances
    def featureImportance: Map[Int, Double] = {
        // gather all nodes
        val nodesNotLeaves = MutableArrayBuffer[Node]()

        def findNodesNotLeaves(current: Node): Unit = {
            if (!current.isEmptyNode && !current.isLeaf) {
                nodesNotLeaves.append(current)
                findNodesNotLeaves(current.getLeftChild)
                findNodesNotLeaves(current.getRightChild)
            }
        }
        findNodesNotLeaves(root)

        // compute importance
        val importances = MutableMap[Int, Double]()
        nodesNotLeaves.foreach{ node => {
            importances += node.getFeatureIndex -> node.nbSamples * node.impurity 
                                                - node.getLeftChild.nbSamples * node.getLeftChild.impurity
                                                - node.getRightChild.nbSamples * node.getRightChild.impurity
        }}
        if (importances.size > 0) {
            val norm = importances.map(_._2).reduce(_ + _)
            importances.map(s => (s._1, s._2/norm)).toMap
        } else Map[Int, Double]()
        
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

}