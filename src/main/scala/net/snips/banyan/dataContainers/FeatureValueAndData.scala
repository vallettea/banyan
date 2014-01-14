package net.snips.banyan.dataContainers

// A class to store the (Feature value, count, y-value sum, y^2-value sum).
case class FeatureValueAndData(

    featureValue: FeatureValue,

    featureCount: Int, 

    yValSum: Double, 

    ySqValSum: Double
    
) {
  
    // Sum the respective fields.
    def +(that: FeatureValueAndData): FeatureValueAndData = {
        new FeatureValueAndData(that.featureValue,
            featureCount + that.featureCount,
            yValSum + that.yValSum,
            ySqValSum + that.ySqValSum)
    }
  
    def canEqual(other: Any) = other.isInstanceOf[FeatureValueAndData]

    override def equals(other: Any) = other match {
        case other: FeatureValueAndData =>
          (other.canEqual(this)) &&
          (featureValue == other.featureValue) &&
          (featureCount == other.featureCount) &&
          (yValSum == other.yValSum)
        case _ => false
    }
  
    override def toString: String = "(%s, %d, %f)".format(
        featureValue.toString(), featureCount, yValSum)
  
    // Return a new object, with the respective data fields summed. The FeatureValue
    // selected is the one corresponding to the parameter. Used for accumulating
    // statistics.
    def accumulate(that: FeatureValueAndData): FeatureValueAndData = {
        new FeatureValueAndData(that.featureValue, featureCount + that.featureCount,
            yValSum + that.yValSum, ySqValSum + that.ySqValSum)
    }
  
    // Return a function for accumulate statistics by incorporating an additional
    // point using the data for a particular feature index.
    def accumulate(featureIndex: Int): (Point) => FeatureValueAndData = {
        def accumulateHelper(featureIndex: Int)(that: Point) = {
            new FeatureValueAndData(that.features(featureIndex), featureCount + 1,
                yValSum + that.yValue, ySqValSum + that.yValue * that.yValue)
        }
        accumulateHelper(featureIndex)
    }
}