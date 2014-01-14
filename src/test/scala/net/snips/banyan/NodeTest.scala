package net.snips.banyan

import net.snips.banyan.dataContainers.FeatureValue
import net.snips.banyan.nodes._

import org.scalatest._
import scala.collection.mutable.ArrayBuffer

class NodeTest extends FlatSpec with Matchers {

    "Ordered Node - Root Only" should "behave normally" in {
        val head = new OrderedNode(0, 0, 0.5)

        // Check the id assignments along with child getting
        head.getId should be (0)

        // Check isLeaf
        head.isLeaf should be (true)

        // Check isEmptyNode
        head.isEmptyNode should be (false)

        // Node 0
        head.getPrediction(Array()) should be (0.5)
    }

    // Create a function which makes Nodes. If isOrdered is true,
    // the resulting function is a proxy for an OrderedNode constructor; otherwise the
    // resulting function is a proxy for a CategoricalNode constructor.
    def makeNode(isOrdered: Boolean): (Int, Int, Double) => Node = {
        def makeNodeHelper(isOrdered: Boolean)(id: Int, featureIndex: Int, prediction: Double) = {
            if (isOrdered) new OrderedNode(id, featureIndex, prediction)
            else new CategoricalNode(id, featureIndex, prediction)
        }
        makeNodeHelper(isOrdered)
    }

    // Create a tree like the following and check it's usage.
    //      0
    //     / \
    //    1   2
    //   / \
    //  3   4
    //
    // makeNode: a proxy for a Node constructor aka the function of the same name.
    // node0Split: Passed directly to the Node 0 constructor. Determines the node split
    //             value.
    // node1Split: Passed directly to the Node 1 constructor. Determines the node split
    //             value.
    // right:     An array of feature vectors which descend to node 2.
    // leftLeft:  An array of feature vectors which descend to node 3.
    // leftRight: An array of feature vectors which descend to node 4.  
    def BuildAndCheckNode(
        makeNode: (Int, Int, Double) => Node,
        node0Split: ArrayBuffer[FeatureValue],
        node1Split: ArrayBuffer[FeatureValue],
        right: Array[Array[FeatureValue]],
        leftLeft: Array[Array[FeatureValue]],
        leftRight: Array[Array[FeatureValue]]): Unit = {
        val head = makeNode(0, 0, 0.5)
        val headLeft = makeNode(1, 1, 1.5)
        val headRight = makeNode(2, 1, 2.5)
        head.insertChildren(headLeft, headRight, node0Split)
        val headLeftLeft = makeNode(3, 2, 3.5)
        val headLeftRight = makeNode(4, 2, 4.5)
        headLeft.insertChildren(headLeftLeft, headLeftRight, node1Split)

        // Check the id assignments along with child getting
        headLeft.getId should be (head.getLeftChild.getId)
        headRight.getId should be (head.getRightChild.getId)
        headLeftLeft.getId should be (head.getLeftChild.getLeftChild.getId)
        headLeftRight.getId should be (head.getLeftChild.getRightChild.getId)

        // Check isLeaf
        head.isLeaf should be (false)
        head.getLeftChild.isLeaf should be (false)
        head.getRightChild.isLeaf should be (true)    
        head.getLeftChild.getLeftChild.isLeaf should be (true) 
        head.getLeftChild.getRightChild.isLeaf should be (true)

        // Check isEmptyNode
        head.isEmptyNode should be (false)
        head.getLeftChild.isEmptyNode should be (false)
        head.getRightChild.isEmptyNode should be (false)
        head.getLeftChild.getLeftChild.isEmptyNode should be (false)
        head.getLeftChild.getRightChild.isEmptyNode should be (false)

        // Node 2
        for (r <- right) head.getPrediction(r) should be (2.5)
          
        // Node 3
        for (ll <- leftLeft) head.getPrediction(ll) should be (3.5)
         
        // Node 4
        for (lr <- leftRight) head.getPrediction(lr) should be (4.5)

        // Check Insufficiently Filled vectors, e.g. only descend to node 1
        for (ll <- leftLeft) head.getPrediction(Array(ll(0))) should be (0.0)
    }

    "Ordered Node" should "behave normally" in {
    // Integer Ordered tree
    BuildAndCheckNode(
        makeNode(true),  // Ordered node
        ArrayBuffer(new FeatureValue(10)),
        ArrayBuffer(new FeatureValue(40)),
        Array(
            Array(new FeatureValue(15))),  // right
        Array(
            Array(new FeatureValue(5),
                new FeatureValue(35))),    // leftLeft
        Array(
            Array(new FeatureValue(5),
                new FeatureValue(45))))   // leftRight
      
    // Double Ordered tree
    BuildAndCheckNode(
        makeNode(true),  // Ordered node
        ArrayBuffer(new FeatureValue(10.0)),
        ArrayBuffer(new FeatureValue(40.0)),
        Array(
            Array(new FeatureValue(15.0))),  // right
        Array(
            Array(new FeatureValue(5.0),
                new FeatureValue(35.0))),    // leftLeft
        Array(
            Array(new FeatureValue(5.0),
                new FeatureValue(45.0))))    // leftRight
            
    // String Ordered tree
    BuildAndCheckNode(
        makeNode(true),  // Ordered node
        ArrayBuffer(new FeatureValue("ab")),
        ArrayBuffer(new FeatureValue("de")),
        Array(
            Array(new FeatureValue("ac"))),  // right
        Array(
            Array(new FeatureValue("aa"),
                new FeatureValue("dc"))),    // leftLeft
        Array(
            Array(new FeatureValue("aa"),
                new FeatureValue("df"))))    // leftRight
    }

    "Categorical Node" should "behave normally" in {
    // Integer Category tree
    BuildAndCheckNode(
        makeNode(false),  // Categorical node
        ArrayBuffer(new FeatureValue(1),
            new FeatureValue(2)),  // Node 0 split
        ArrayBuffer(new FeatureValue(3),
            new FeatureValue(4)),  // Node 1 split
        Array(
            Array(new FeatureValue(3)),
            Array(new FeatureValue(4))),  // right
        Array(
            Array(new FeatureValue(1),
                new FeatureValue(3)),
             Array(new FeatureValue(2),
                new FeatureValue(4))),   // leftLeft
        Array(
            Array(new FeatureValue(1),
                new FeatureValue(1))))   // leftRight
                
    // String Category tree
    BuildAndCheckNode(
        makeNode(false),  // Categorical node
        ArrayBuffer(new FeatureValue("a"),
            new FeatureValue("b")),  // Node 0 split
        ArrayBuffer(new FeatureValue("c"),
            new FeatureValue("d")),  // Node 1 split
        Array(
            Array(new FeatureValue("c")),
            Array(new FeatureValue("d"))),  // right
        Array(
            Array(new FeatureValue("a"),
                new FeatureValue("d")),
             Array(new FeatureValue("b"),
                new FeatureValue("c"))),   // leftLeft
        Array(
            Array(new FeatureValue("a"),
                new FeatureValue("a"))))   // leftRight      
    }

    "Unstructured Node" should "behave normally" in {
        val nodeMaker = makeNode(true)
        val head = nodeMaker(0, 0, 0.5)
        val headLeft = new UnstructuredNode(1.5, head, 0)
        val headRight = new UnstructuredNode(2.5, head, 0)
        head.insertChildren(headLeft, headRight, ArrayBuffer(new FeatureValue(10)))
        val newLeftNode = nodeMaker(3, 1, 3.5)
        val newRightNode = nodeMaker(4, 1, 4.5)

        val leftFeatures = Array(new FeatureValue(5))
        val rightFeatures = Array(new FeatureValue(15))

        assert(1.5 === head.getPrediction(leftFeatures))
        assert(2.5 === head.getPrediction(rightFeatures))

        headLeft.replaceNode(newLeftNode)
        assert(3.5 === head.getPrediction(leftFeatures))
        assert(2.5 === head.getPrediction(rightFeatures))       

        headRight.replaceNode(newRightNode)
        assert(3.5 === head.getPrediction(leftFeatures))
        assert(4.5 === head.getPrediction(rightFeatures))      
    }
}