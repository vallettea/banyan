package net.snips.banyan.dataContainers

// The type of a feature, either ordered or categorical.
//
// isOrdered: Indicates whether the feature is ordered or not (categorical).
// values:    A set of permissible values for this feature. For ordered
//            features, this serves mainly to indicate the data type.
case class FeatureType(

    isOrderedFeature: Boolean, 

    values: Set[FeatureValue]

) {

    def getCategoricalValues: Set[FeatureValue] = values
    def getOrderedValue: FeatureValue = values.head

    def isOrdered: Boolean = isOrderedFeature
    def isCategorical: Boolean = !isOrderedFeature

    // Validates the feature type. For categorical features, this ensures that
    // there is at least one value and that all values are of the same type.
    // Ordered features are valid by default.
    def validate: Boolean = {
        if (isCategorical) {
            if (values.isEmpty) false
            else {
                var haveSameType = true
                var prev = values.head
                for (current <- values) {
                    haveSameType &&= prev.hasSameType(current)
                    prev = current
                }
                haveSameType
            }
        } else true
    }

}