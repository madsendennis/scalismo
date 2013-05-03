package smptk
package registration

import image.CoordVector
import image.Geometry.{CoordVector1D, CoordVector2D, CoordVector3D}
import common.BoxedRegion
import breeze.linalg.DenseVector
import breeze.linalg.DenseMatrix
import smptk.common.DiscreteDomain
import smptk.common.{BoxedRegion1D, BoxedRegion2D, BoxedRegion3D}
import smptk.numerics.{UniformSampler1D, UniformSampler2D, UniformSampler3D}
import smptk.numerics.Sampler
import smptk.registration.Kernel


case class GaussianProcess[CV[A] <: CoordVector[A]](val domain: BoxedRegion[CV], val mean: CV[Double] => DenseVector[Double], val cov: PDKernel[CV]) {

  type PointSample = IndexedSeq[CV[Double]]

  def sample: (PointSample => IndexedSeq[DenseVector[Double]]) = { (xs: PointSample) =>
    {
      val n = xs.size
      val d = cov.outputDim
      val meanVec = DenseVector.zeros[Double](n * d)
      for (i <- 0 until n; di <- 0 until d) meanVec(i * d + di) = mean(xs(i))(di)
      val covMatrix = Kernel.computeKernelMatrix(xs, cov)
      val noise = breeze.linalg.diag(DenseVector.ones[Double](xs.size)) * 1e-6 // gaussian noise for stability 
      val lMat = breeze.linalg.cholesky(covMatrix + noise)
      val u = for (_ <- 0 until xs.size) yield breeze.stats.distributions.Gaussian(0, 1).draw()
      val uVec = DenseVector(u.toArray)
      val sampleVec = meanVec + lMat * uVec // the sample as a long vector
      
      // now we group it 
      // TODO make me more efficient and elegant
      val sampleSeq = sampleVec.toArray.toIndexedSeq
      val pointSampleValues = sampleSeq.grouped(d).map(ptVec => DenseVector(ptVec.toArray))
      pointSampleValues.toIndexedSeq
    }
  }
}

trait LowRankGaussianProcess[CV[A] <: CoordVector[A]] {

  def makeUniformSampler(numPoints : Int) : Sampler[CV]
  val domain : BoxedRegion[CV]
  val m : CV[Double] => DenseVector[Double]
  val k : PDKernel[CV] 
  
  val (eigenPairs, n) = Kernel.computeNystromApproximation(k, domain, 20, makeUniformSampler(500))

  def instance(alpha : DenseVector[Double]) : CV[Double] => DenseVector[Double] = {
    x =>
    {
      eigenPairs.zipWithIndex.par.map(eigPairWithIndex => {
        val ((lambda, phi), i) = eigPairWithIndex
        phi(x) * alpha(i) * math.sqrt(lambda)
      }).foldLeft(m(x))(_ + _)
    }
    
  } 
  def sample: CV[Double] => DenseVector[Double] = { 
      val coeffs = for (_ <- 0 until n) yield breeze.stats.distributions.Gaussian(0, 1).draw()
      instance(DenseVector(coeffs.toArray))
  }

}


case class LowRankGaussianProcess1D(val domain: BoxedRegion1D, val m: CoordVector1D[Double] => DenseVector[Double], val k: PDKernel[CoordVector1D]) 
extends LowRankGaussianProcess[CoordVector1D] {
	
	def makeUniformSampler(numPoints : Int) = UniformSampler1D(numPoints)
}


case class LowRankGaussianProcess2D(val domain: BoxedRegion2D, val m: CoordVector2D[Double] => DenseVector[Double], val k: PDKernel[CoordVector2D]) 
extends LowRankGaussianProcess[CoordVector2D] {
	
	def makeUniformSampler(numPoints : Int) = UniformSampler2D(numPoints)
}

case class LowRankGaussianProcess3D(val domain: BoxedRegion3D, val m: CoordVector3D[Double] => DenseVector[Double], val k: PDKernel[CoordVector3D]) 
extends LowRankGaussianProcess[CoordVector3D] {
	
	def makeUniformSampler(numPoints : Int) = UniformSampler3D(numPoints)
}



object GaussianProcess { 
  
        
  def regression[CV[A] <: CoordVector[A]](gp : GaussianProcess[CV], trainingData: IndexedSeq[(CV[Double], DenseVector[Double])], sigma2: Double)  = {

    def flatten(v : IndexedSeq[DenseVector[Double]]) = DenseVector(v.flatten(_.toArray).toArray)
    val d = gp.cov.outputDim
    val (xs, ys) = trainingData.unzip  
    val yVec= flatten(ys)
    val meanValues = xs.map(gp.mean(_))
    val mVec = flatten(meanValues)
    val kxx = Kernel.computeKernelMatrix(xs, gp.cov)

    val kinv = breeze.linalg.inv(kxx + DenseMatrix.eye[Double](kxx.cols) * sigma2)

    def mp(x: CV[Double]): DenseVector[Double] = {
      val kxs = Kernel.computeKernelVectorFor(x, xs, gp.cov)      
      gp.mean(x) + (kxs * (kinv * (yVec - mVec)))

    }

    val kp = new PDKernel[CV] {
      def apply(x1: CV[Double], x2: CV[Double]) : DenseMatrix[Double] = {
        val kx1xs = Kernel.computeKernelVectorFor(x1, xs, gp.cov)
        val kx2xs = Kernel.computeKernelVectorFor(x2, xs, gp.cov)
        gp.cov(x1, x2) - (kx1xs * (kinv * kx2xs.t))
      }
      def outputDim = d
    }
   GaussianProcess(gp.domain, mp, kp)
  }
}