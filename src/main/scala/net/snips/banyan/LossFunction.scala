package  net.snips.banyan

import scala.math


abstract class LossFunction {
  
    // Get the loss where residual = actual - predicted.
    def getLoss(residual: Double): Double

    // Get the negative derivative of the loss at the
    // residual where residual = actual - predicted.
    def getNegDerivative(residual: Double): Double

    // Return an optional parameter. Used for Huber Loss.
    def getParameter: Double = 0.0

}


class SquaredLoss extends LossFunction {
  
    def getLoss(residual: Double) = residual * residual / 2.0
    
    def getNegDerivative(residual: Double) = residual
}


class AbsoluteLoss extends LossFunction {
  
    override def getLoss(residual: Double) = math.abs(residual)

    override def getNegDerivative(residual: Double) = Math.signum(residual)
}

// TODO: Build a delta auto-tune so that we can set it to the alpha-quantile
// across all data.
class HuberLoss(val delta: Double) extends LossFunction {
  
    override def getLoss(residual: Double) = {
        val absResidual = math.abs(residual)
        if (absResidual <= delta) residual * residual / 2.0
        else delta * (absResidual - deltaHalf)
    }

    override def getNegDerivative(residual: Double) = {
        val absResidual = math.abs(residual)
        if (absResidual <= delta) residual
        else delta * math.signum(residual)
    }

    override def getParameter: Double = delta

    private val deltaHalf: Double = delta / 2.0
}