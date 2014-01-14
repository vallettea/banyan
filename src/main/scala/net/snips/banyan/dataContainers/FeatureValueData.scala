package net.snips.banyan.dataContainers

// A class to store the (count, y-value sum, y^2-value sum) for each
// matching feature value.
case class FeatureValueData(

    featureCount: Int,

    yValSum: Double,

    ySqValSum: Double
    
) {

    // Add two FeatureValueData objects by summing the respective fields.
    def +(that: FeatureValueData): FeatureValueData = {
        new FeatureValueData(featureCount + that.featureCount,
            yValSum + that.yValSum,
            ySqValSum + that.ySqValSum)
    }

    // Accumulate another y-value.
    def accumulate(thatYVal: Double): FeatureValueData = {
        new FeatureValueData(featureCount + 1, yValSum + thatYVal,
            ySqValSum + thatYVal * thatYVal)
    }
 
    override def toString: String = "(%d, %f)".format(featureCount, yValSum)

    def canEqual(other: Any) = other.isInstanceOf[FeatureValueData]

    override def equals(other: Any) = other match {
        case other: FeatureValueData =>
          (other.canEqual(this)) &&
          (featureCount == other.featureCount) &&
          (yValSum == other.yValSum)
        case _ => false
    }
}