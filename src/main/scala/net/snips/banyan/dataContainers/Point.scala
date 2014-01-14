package net.snips.banyan.dataContainers

// A class representing an atom of training data.
case class Point(

    features: Array[FeatureValue],
    
    yValue: Double

) {
  
  override def toString: String = "(%s => %f)".format(
        features.map(_.toString).reduce(_ + ", " + _),
        yValue
  )

}