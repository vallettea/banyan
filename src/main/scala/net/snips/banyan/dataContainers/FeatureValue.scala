package net.snips.banyan.dataContainers

// A wrapper class for different types of feature x-values.
// FeatureValue instantiations are comparable.
case class FeatureValue(value: Any) extends Ordered[FeatureValue] {

    def isDouble: Boolean = value.isInstanceOf[Double]
    def isInt: Boolean = value.isInstanceOf[Int]
    def isString: Boolean = value.isInstanceOf[String]

    def getDouble: Double = value.asInstanceOf[Double]
    def getInt: Int = value.asInstanceOf[Int]
    def getString: String = value.asInstanceOf[String]

    def hasSameType(that: FeatureValue): Boolean = value.getClass() == that.value.getClass()

    override def compare(other: FeatureValue): Int = other.value match {
        case that: Double => if (isDouble) getDouble.compare(that) else -1
        case that: Int => if (isInt) getInt.compare(that) else -1
        case that: String => if (isString) getString.compare(that) else -1
        case _ => -1
    }

    def canEqual(other: Any): Boolean = other.isInstanceOf[FeatureValue]

    override def equals(other: Any) = other match {
        case that: FeatureValue => (that canEqual this) && compare(that) == 0
        case _ => false
    }

    override def hashCode = value.hashCode

    override def toString = value.toString
}

object FeatureValue {
    def defaultValue: FeatureValue = new FeatureValue(0)
}
