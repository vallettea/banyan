package net.snips.banyan.dataContainers

import org.scalatest._

class FeatureTest extends FlatSpec with Matchers {
  
    "Same Integers" should "be equal" in {
        val feature = new FeatureValue(3)
        new FeatureValue(3) should be (new FeatureValue(3))
        new FeatureValue(3) should be (feature)
        feature should be (new FeatureValue(3))
    }

    "Different Integers" should  "not be equal" in {
        new FeatureValue(3) should not be (new FeatureValue(2))
    }

    "Different Types" should "not be equal" in {
        new FeatureValue("a") should not be (new FeatureValue(2))
        new FeatureValue("abc") should not be (new FeatureValue(3.0))
        new FeatureValue(3) should not be (new FeatureValue(2.0))        
    }

    def comparisonTest[A](smaller: A, larger: A): Unit = {
        assert(new FeatureValue(smaller) <= new FeatureValue(smaller))
        assert(new FeatureValue(smaller) >= new FeatureValue(smaller))    
        assert(new FeatureValue(smaller) < new FeatureValue(larger))
        assert(new FeatureValue(smaller) <= new FeatureValue(larger))    
        assert(new FeatureValue(larger) > new FeatureValue(smaller))
        assert(new FeatureValue(larger) >= new FeatureValue(smaller))    
    }

    "Integer Comparison" should "work" in {
        comparisonTest(1, 2)
    }

    "Double Comparison" should "work" in {
        comparisonTest(1.0, 2.0)
    }

    "String comparison" should "work" in {
        comparisonTest("abc", "abd")
    }

    "Categorical Feature Type" should "behave normally" in {
        val categories = Set(new FeatureValue(1),
            new FeatureValue(2),
            new FeatureValue(3))
        val categoricalType = new FeatureType(false, categories)
        categoricalType.isCategorical should be (true)
        categoricalType.isOrdered should be (false)
        categories should be (categoricalType.getCategoricalValues)
        categoricalType.validate should be (true)
    }

    "Categorical Feature Type " should "fail" in {
        val categories = Set(new FeatureValue(1),
            new FeatureValue(2.0),
            new FeatureValue(3))
        val categoricalType = new FeatureType(false, categories)
        categoricalType.isCategorical should be (true)
        categoricalType.isOrdered should be (false)
        categoricalType.validate should be (false)
    }  

    "Ordered Feature Type" should "behave normally" in {
        val orderedType = new FeatureType(true, null)
        orderedType.isCategorical should be (false)
        orderedType.isOrdered should be (true)
        orderedType.validate should be (true)
    }  
}