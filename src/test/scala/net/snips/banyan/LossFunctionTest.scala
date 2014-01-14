package net.snips.banyan

import org.scalatest._

class LossFunctionTest extends FlatSpec with Matchers {

	it should "Compute squared loss" in {
		val loss: LossFunction = new SquaredLoss
		assert(0.0 === loss.getLoss(0.0))
		assert(2.0 === loss.getLoss(2.0))
		assert(2.0 === loss.getLoss(-2.0))
		assert(8.0 === loss.getLoss(-4.0))

		assert(0.0 === loss.getNegDerivative(0.0)) 
		assert(2.0 === loss.getNegDerivative(2.0))
		assert(-2.0 === loss.getNegDerivative(-2.0))
		assert(-4.0 === loss.getNegDerivative(-4.0))
	}

	it should "compute absolute loss" in {
		val loss: LossFunction = new AbsoluteLoss
		assert(0.0 === loss.getLoss(0.0))
		assert(2.0 === loss.getLoss(2.0))
		assert(2.0 === loss.getLoss(-2.0))
		assert(4.0 === loss.getLoss(-4.0))

		assert(0.0 === loss.getNegDerivative(0.0)) 
		assert(1.0 === loss.getNegDerivative(2.0))
		assert(-1.0 === loss.getNegDerivative(-2.0))
		assert(-1.0 === loss.getNegDerivative(-4.0))
	}

	it should "compute Huber loss" in {
		val loss: LossFunction = new HuberLoss(2.0)
		assert(0.0 === loss.getLoss(0.0))
		assert(2.0 === loss.getLoss(2.0))
		assert(2.0 === loss.getLoss(-2.0))
		assert(6.0 === loss.getLoss(4.0))    
		assert(6.0 === loss.getLoss(-4.0))

		assert(0.0 === loss.getNegDerivative(0.0))    
		assert(2.0 === loss.getNegDerivative(2.0))
		assert(-2.0 === loss.getNegDerivative(-2.0))
		assert(2.0 === loss.getNegDerivative(4.0))
		assert(-2.0 === loss.getNegDerivative(-4.0))
	}  
}