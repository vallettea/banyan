package net.snips.banyan.nodes

import net.snips.banyan.dataContainers.FeatureValue


class EmptyNode extends Node {
    override def getLeaf(features : Array[FeatureValue]): Node = this
    override def getLeftChild: Node = this
    override def getRightChild: Node = this
    override def isEmptyNode: Boolean = true
}

object EmptyNode {
    def getEmptyNode: Node = emptyNode
    val emptyNode: EmptyNode = new EmptyNode
}
