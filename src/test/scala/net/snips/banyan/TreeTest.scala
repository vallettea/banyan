package net.snips.banyan

import net.snips.banyan.dataContainers.FeatureValue
import net.snips.banyan.nodes._

import org.scalatest._
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashSet

class TreeTest extends FlatSpec with Matchers {
  
    "A tree" should "have retrieve no leaf nodes when empty" in {
        val tree = new Tree(1, 5)
        val leaves = tree.getLeaves
        leaves should be (ArrayBuffer())
    }

    // Create a tree like the following and check it's usage.
    //      0
    //     / \
    //    1   2
    //   / \
    //  3   4
    //     / \
    //    5   6
    it should "retrieve the correct leaves" in {
        val tree = new Tree(1.0, 6)
        val head = new OrderedNode(0, 0, 0.5, 0, 0)  
        tree.setRootNode(head)

        val headL = new OrderedNode(1, 1, 1.5, 0, 0)
        val headR = new OrderedNode(2, 1, 2.5, 0, 0)
        tree.insertChildren(head, headL, headR, ArrayBuffer(new FeatureValue(10)))
        tree.isFull should be (false)

        val headLL = new OrderedNode(3, 2, 3.5, 0, 0)
        val headLR = new OrderedNode(4, 2, 4.5, 0, 0)
        tree.insertChildren(headL, headLL, headLR, ArrayBuffer(new FeatureValue(20)))
        tree.isFull should be (false)
            
        val headLRL = new OrderedNode(5, 3, 5.5, 0, 0)
        val headLRR = new OrderedNode(6, 3, 6.5, 0, 0)
        tree.insertChildren(headLR, headLRL, headLRR, ArrayBuffer(new FeatureValue(30)))
        tree.isFull should be (true)

        val leaves = tree.getLeaves
        val leafNodeIds = new HashSet[Int]
        leaves.foreach(leaf => leafNodeIds.add(leaf.getId))
        leafNodeIds should be (Set(2, 3, 5, 6))
    }
}