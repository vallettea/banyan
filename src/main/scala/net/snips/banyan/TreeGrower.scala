package net.snips.banyan

import net.snips.banyan.dataContainers.{FeatureValue, FeatureType, Point}
import net.snips.banyan.nodeSplitter._
import net.snips.banyan.nodes._

import scala.collection.mutable.{ArrayBuffer => MutableArrayBuffer}
import scala.collection.mutable.{Map => MutableMap, HashMap => MutableHashMap}
import scala.reflect.BeanProperty

// A class for growing the number of nodes in a tree.
//
// tree:          The tree to grow.
// featureTypes:  An array of FeatureTypes (ordered or categorical),
//                in the same order as a feature vector.
// trainingData: An array of training data.
case class TreeGrower(

    tree: Tree,

    featureTypes: Array[FeatureType],

    trainingData: Array[Point]

) {
  
    // Grow the tree until it has a sufficient number of nodes.
    def grow(): Unit = {

        setRootNodeIfNecessary(trainingData)

        val bestSplitForNode = MutableHashMap[Node, BestSplit]()
        var done = false

        while (!tree.isFull && !done) {

            val nodesToInvestigate = findNodesToInvestigate()

            if (!nodesToInvestigate.isEmpty) {
                trainingData.foreach{ point => {
                    val leaf = tree.getLeaf(point.features)
                    if (nodesToInvestigate.contains(leaf)) {
                        val nodeSplit = nodesToInvestigate(leaf)
                        nodeSplit.process(point)
                    }
                }}
                while (!nodesToInvestigate.isEmpty) {
                    val (node, nodeSplit) = nodesToInvestigate.head
                    bestSplitForNode.put(node, nodeSplit.findBestSplit())
                    // println((node, nodeSplit.findBestSplit()))
                    nodesToInvestigate.remove(node)
                }
            }

            if (bestSplitForNode.isEmpty) done = true
            else {
                val (node, bestSplit) = bestSplitForNode.minBy(_._2)
                if (bestSplit.isNotASolution) done = true
                else {
                    growTreeAtNode(node, bestSplit)
                    bestSplitForNode.remove(node)
                }
            }

        }

    }
  
  
    private def setRootNodeIfNecessary(trainingData: Array[Point]): Unit = {
        if (tree.size == 0) {

            val yValSum = trainingData.map(point => point.yValue).reduce( _ + _)
            val numPoints = trainingData.size
            val average = if (numPoints > 0) yValSum / numPoints else 0.0

            tree.setRootNode(new UnstructuredNode(average, EmptyNode.getEmptyNode, Double.PositiveInfinity))
        }
    }
  
    // Nodes which have a sufficiently small prediction error should
    // be ignored in growing the tree.
    private def shouldIgnoreNode(node: Node): Boolean = node.getError == 0.0


    // Find nodes for which we have not yet determined a best split. If the tree
    // is empty, a map with an EmptyNode as a key is returned.
    private def findNodesToInvestigate(): MutableMap[Node, NodeSplit] = {

        val leaves: MutableArrayBuffer[Node] = tree.getLeaves
        if (leaves.isEmpty) leaves.append(EmptyNode.getEmptyNode)

        val nodesToInvestigate = new MutableHashMap[Node, NodeSplit]
        for (node <- leaves) {
            if (!bestSplitForNode.contains(node) && !shouldIgnoreNode(node))
                nodesToInvestigate.put(node, new NodeSplit(featureTypes))
        }
        nodesToInvestigate
    }
  
    // All leaves are UnstructuredNodes. If and when, we grow the tree at
    // a leaf, we need to change the node to an ordered or categorical node
    // since we now know what feature we are using to split. If node is
    // an instance of EmptyNode, the tree is empty so we grow the tree at the
    // root.
    //
    // node: The node to replace. By construction, it will be an Unstructured
    //       Node.
    // bestSplit: The data for splitting the node.
    private def replaceNode(node: Node, bestSplit: BestSplit): Node = {

        if (node.isEmptyNode) {
            val newNode = new UnstructuredNode(bestSplit.leftPrediction, node, bestSplit.error)
            tree.setRootNode(newNode)
            newNode
        } else {
            val newNode = {
                if (featureTypes(bestSplit.featureIndex).isOrdered) {
                    new OrderedNode(nextId,
                        bestSplit.featureIndex,
                        node.getNodePrediction,
                        bestSplit.error,
                        bestSplit.totCount
                    )
                } else {
                    new CategoricalNode(nextId,
                        bestSplit.featureIndex,
                        node.getNodePrediction,
                        bestSplit.error,
                        bestSplit.totCount
                    )
                }
            }
            nextId += 1
            if (tree.size == 1) tree.replaceRootNode(newNode)
            else node.replaceNode(newNode)
            newNode
        }

    } 
  
    // Increase the size of the tree by two, by growing a left and
    // right child.
    //
    // node:      The leaf node at which to grow children.
    // bestSplit: Data about how to split the node.
    private def growTreeAtNode(node: Node, bestSplit: BestSplit): Unit = {
        val parent = replaceNode(node, bestSplit)
        // If node is an EmptyNode, we have no children to insert.
        if (!node.isEmptyNode) {
            val leftChild = new UnstructuredNode(bestSplit.leftPrediction, parent, bestSplit.leftError)
            val rightChild = new UnstructuredNode(bestSplit.rightPrediction, parent, bestSplit.rightError)
            tree.insertChildren(parent, leftChild, rightChild, bestSplit.splitFeatures)
        }
    }

    // A map from a Node to the Best Split for the node. Each element of
    // the array is a split based on a different feature.
    private val bestSplitForNode = new MutableHashMap[Node, BestSplit]

    // The next id to use for creating a new node.
    private var nextId = 1
}