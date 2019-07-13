/*
 * Copyright 2015 University of Basel, Graphics and Vision Research Group
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package scalismo.registration

import breeze.linalg.{DenseMatrix, DenseVector}
import scalismo.common._
import scalismo.geometry
import scalismo.geometry._
import scalismo.registration.TransformationSpace.ParameterVector
import scalismo.utils.Memoize

import scala.annotation._

/**
 * Trait for D-dimensional transformation that maps a D-dimensional Point to another.
 *  A transformation in our library is seen as a particular type of Field (or image)  mapping points
 *  to values that are also of type [[scalismo.geometry.Point]]
 */
trait Transformation[D] extends Field[D, Point[D]] {}
/** Trait for parametric D-dimensional transformation */

object Transformation {

  /**
   * Create a transformation defined on the whole real space with the given function
   */
  def apply[D](t: Point[D] => Point[D]): Transformation[D] = {
    new Transformation[D] {
      override val f: (Point[D]) => Point[D] = t

      override def domain: Domain[D] = RealSpace[D]
    }
  }

  /**
   * Returns a new transformation that memoizes (caches) the values that have already been
   * computed. The size of the cache used is given by the argument cacheSizeHint.
   */
  def memoize[D](t: Transformation[D], cacheSizeHint: Int) = new Transformation[D] {
    override protected[scalismo] val f: (Point[D]) => Point[D] = Memoize(t.f, cacheSizeHint)
    override def domain: Domain[D] = t.domain
  }

}

trait ParametricTransformation[D] extends Transformation[D] {
  /** the parameters defining the transform*/
  val parameters: TransformationSpace.ParameterVector
}

/** Trait for invertible D-dimensional transformation */
trait CanInvert[D] {
  self: Transformation[D] =>
  def inverse: Transformation[D]
}
/** Trait for differentiable D-dimensional transformation */
trait CanDifferentiate[D] {
  self: Transformation[D] =>
  /** Derivative of the transform evaluated at a point */
  def takeDerivative(x: Point[D]): SquareMatrix[D]
}
/**
 * Trait for a parametric transformation space.
 *
 *  Many pre-implemented transformation spaces implement this trait : [[TranslationSpace]], [[ScalingSpace]], [[RotationSpace]] ..
 *
 *  Most of the basic transforms in our library (scaling, translation, rotation ..) can be created directly and do not necessarily require
 *  the prior creation of a TransformationSpace.
 *  Defining a TransformationSpace is mainly useful during a registration process where one optimizes over a set of parameters to retrieve
 *  a desired transformation.
 *
 */
trait TransformationSpace[D] {
  /** Type of parametric transforms generated by this space*/
  type T <: ParametricTransformation[D]

  type JacobianImage = Point[D] => DenseMatrix[Double]

  def parametersDimensionality: Int
  /** returns a function providing the Jacobian at a requested point. This is used when optimizing over the parameters, e.g. during registration*/
  def takeDerivativeWRTParameters(alpha: ParameterVector): JacobianImage
  /** returns a transformation corresponding the indicated parameters*/
  def transformForParameters(p: ParameterVector): T
  /** returns the parameters corresponding the identity transform in this space*/
  def identityTransformParameters: DenseVector[Double]
}

object TransformationSpace {
  /** Type alias for parameters used with parametric transformations spaces. Currently, these are [[breeze.linalg.DenseVector]]*/
  type ParameterVector = DenseVector[Double]
}

/** Trait for a parametric transformation space returning differentiable transforms*/
trait DifferentiableTransforms[D] { self: TransformationSpace[D] =>

  override type T <: ParametricTransformation[D] with CanDifferentiate[D]
  /** Produces a ProductTransformationSpace combining self with the space passed as argument*/
  def product(that: TransformationSpace[D] with DifferentiableTransforms[D]) = {
    new ProductTransformationSpace(this, that)
  }
}
/**
 * Parametric Transformation Space Combining two transformation spaces passed as arguments
 *
 * @param outer Parametric transformation space generating the transforms to be applied second
 * @param inner Parametric transformation space generating the transforms to be applied first
 * @tparam D Dimensionality of the inputs to the transformations generated by this space
 * @tparam OT Type of the outer transformation space. This space must generate differentiable parametric transforms
 * @tparam IT Type of the inner transformation space. This space must generate differentiable parametric transforms
 */
class ProductTransformationSpace[D, OT <: ParametricTransformation[D] with CanDifferentiate[D], IT <: ParametricTransformation[D] with CanDifferentiate[D]] private[registration] (outer: TransformationSpace[D] with DifferentiableTransforms[D], inner: TransformationSpace[D] with DifferentiableTransforms[D]) extends TransformationSpace[D] with DifferentiableTransforms[D] {

  override type T = CompositeTransformation[D]

  def parametersDimensionality = outer.parametersDimensionality + inner.parametersDimensionality

  def identityTransformParameters = DenseVector.vertcat(outer.identityTransformParameters, inner.identityTransformParameters)

  /**Returns a transform belonging to the product space. Parameters should be a concatenation of the outer and inner space parameters*/
  override def transformForParameters(p: ParameterVector) = {
    val (outerParams, innerParams) = splitProductParameterVector(p)
    new CompositeTransformation(outer.transformForParameters(outerParams), inner.transformForParameters(innerParams))
  }

  override def takeDerivativeWRTParameters(p: ParameterVector) = {

    val split = splitProductParameterVector(p)

    (x: Point[D]) => {
      DenseMatrix.horzcat(
        outer.takeDerivativeWRTParameters(split._1)(x),
        outer.transformForParameters(split._1).takeDerivative(inner.transformForParameters(split._2)(x)).toBreezeMatrix * inner.takeDerivativeWRTParameters(split._2)(x))
    }
  }

  protected def splitProductParameterVector(p: ParameterVector): (ParameterVector, ParameterVector) = {

    val pthisD = DenseVector(p.data.take(outer.parametersDimensionality))
    val pthatD = DenseVector(p.data.drop(outer.parametersDimensionality))

    (pthisD, pthatD)
  }
}
/**
 *  Class defining transformations composed of two argument transforms.
 *  The resulting transform is <code>outerTransform compose innerTransform</code>
 *
 *  @param innerTransform transform to be applied first. Must be a parametric differentiable transform
 *  @param outerTransform transform to be applied second. Must be a parametric differentiable transform
 */
case class CompositeTransformation[D](outerTransform: ParametricTransformation[D] with CanDifferentiate[D], innerTransform: ParametricTransformation[D] with CanDifferentiate[D]) extends ParametricTransformation[D] with CanDifferentiate[D] {

  override val domain = innerTransform.domain
  override val f = (x: Point[D]) => {
    (outerTransform compose innerTransform)(x)
  }

  override def takeDerivative(x: Point[D]) = {
    outerTransform.takeDerivative(innerTransform(x)) * innerTransform.takeDerivative(x)
  }
  /** parameters of the composed transform. This is simply a concatenation of the outer and inner transform parameters */
  override val parameters = DenseVector(outerTransform.parameters.data ++ innerTransform.parameters.data)

}

@deprecated("This class will be removed in the next version of scalismo. Please use CompositeTransformation instead.", "0.15.0")
case class ProductTransformation[D](outerTransform: ParametricTransformation[D] with CanDifferentiate[D], innerTransform: ParametricTransformation[D] with CanDifferentiate[D]) extends ParametricTransformation[D] with CanDifferentiate[D] {

  private val compositeTransformation = CompositeTransformation(outerTransform, innerTransform)

  override val parameters: ParameterVector = compositeTransformation.parameters

  /** Derivative of the transform evaluated at a point */
  override def takeDerivative(x: Point[D]): SquareMatrix[D] = compositeTransformation.takeDerivative(x)

  /** The domain on which the image is defined */
  override def domain: Domain[D] = compositeTransformation.domain

  override val f: (Point[D]) => Point[D] = compositeTransformation.f
}

/**
 * Parametric transformation space producing translation transforms
 */
class TranslationSpace[D: NDSpace] private () extends TransformationSpace[D] with DifferentiableTransforms[D] {

  override type T = TranslationTransform[D]

  def parametersDimensionality: Int = implicitly[NDSpace[D]].dimensionality
  override def identityTransformParameters = DenseVector.zeros(parametersDimensionality)
  /**Returns a translation transform, where the translation vectors' coordinates are the given parameters*/
  override def transformForParameters(p: ParameterVector): TranslationTransform[D] = TranslationTransform[D](EuclideanVector[D](p.data))

  override def takeDerivativeWRTParameters(p: ParameterVector) = { x: Point[D] => DenseMatrix.eye[Double](parametersDimensionality) }
}

/** Factory for [[TranslationSpace]] instances. */
object TranslationSpace {
  /** factory method to create a D-dimensional Translation space*/
  def apply[D: NDSpace] = new TranslationSpace[D]

}

/**
 * D-dimensional translation transform that is parametric, invertible and differentiable
 *
 *  @param t Translation vector
 */
case class TranslationTransform[D: NDSpace](t: EuclideanVector[D]) extends ParametricTransformation[D] with CanInvert[D] with CanDifferentiate[D] {
  override val f = (pt: Point[D]) => pt + t
  override val domain = RealSpace[D]
  override def takeDerivative(x: Point[D]): SquareMatrix[D] = SquareMatrix.eye[D]
  override def inverse: TranslationTransform[D] = new TranslationTransform(t * (-1f))
  /**parameters are the coordinates of the translation vector*/
  val parameters = t.toBreezeVector
}

/**
 * Parametric transformation space producing rotation transforms around a rotation centre.
 *
 */
abstract class RotationSpace[D: NDSpace] extends TransformationSpace[D] with DifferentiableTransforms[D] {

  /** Centre of rotation. All rotations generated by this parametric space will be around this indicated Point*/
  def centre: Point[D]

  override type T = RotationTransform[D]

  /**
   * Returns a rotation transform corresponding to the given parameters.
   *
   *  Rotation parameters for _2D : a scalar indicating the rotation angle in radians
   *  Rotation parameters for _3D : Euler angles in x-convention (rotation around Z axis first, around Y axis second, around X axis third)
   */
  override def transformForParameters(p: ParameterVector): RotationTransform[D]

  override def identityTransformParameters = DenseVector.zeros[Double](parametersDimensionality)
}

private class RotationSpace1D extends RotationSpace[_1D] {

  override def centre = Point(0)
  override def parametersDimensionality: Int = 1

  override def transformForParameters(p: ParameterVector): RotationTransform[_1D] = {

    RotationTransform1D()
  }

  override def takeDerivativeWRTParameters(p: ParameterVector) = {
    (x: Point[_1D]) => DenseMatrix.zeros[Double](0, 0)
  }
}

private class RotationSpace2D(val centre: Point[_2D]) extends RotationSpace[_2D] {

  override def parametersDimensionality: Int = 1

  override def transformForParameters(p: ParameterVector): RotationTransform[_2D] = {
    require(p.length == parametersDimensionality)

    val rotMatrix = SquareMatrix(
      (math.cos(p(0)), -math.sin(p(0))),
      (math.sin(p(0)), math.cos(p(0))))

    RotationTransform[_2D](rotMatrix, centre)
  }

  override def takeDerivativeWRTParameters(p: ParameterVector) = {

    val df = (x: Point[_2D]) => {
      val sa = math.sin(p(0))
      val ca = math.cos(p(0))
      val cx = centre(0)
      val cy = centre(1)

      DenseMatrix(
        -sa * (x(0) - cx) - ca * (x(1) - cy),
        ca * (x(0) - cx) - sa * (x(1) - cy))
    }
    df
  }
}

private class RotationSpace3D(val centre: Point[_3D]) extends RotationSpace[_3D] {
  override def parametersDimensionality: Int = 3

  override def transformForParameters(p: ParameterVector): RotationTransform[_3D] = {
    require(p.length == parametersDimensionality)
    val rotMatrix = RotationSpace.eulerAnglesToRotMatrix3D(p)
    RotationTransform[_3D](rotMatrix, centre)
  }

  override def takeDerivativeWRTParameters(p: ParameterVector) = {

    val df = (x: Point[_3D]) => {
      val cospsi = Math.cos(p(2))
      val sinpsi = Math.sin(p(2))
      val costh = Math.cos(p(1))
      val sinth = Math.sin(p(1))
      val cosphi = Math.cos(p(0))
      val sinphi = Math.sin(p(0))

      val x0minc0 = x(0) - centre(0)
      val x1minc1 = x(1) - centre(1)
      val x2minc2 = x(2) - centre(2)

      // 3 by 3 matrix (nbrows=point dim, nb cols = param dim )
      val dr00 = (-sinphi * costh * x0minc0) + (-sinphi * sinpsi * sinth - cospsi * cosphi) * x1minc1 + (sinpsi * cosphi - cospsi * sinth * sinphi) * x2minc2
      val dr01 = -sinth * cosphi * x0minc0 + costh * sinpsi * cosphi * x1minc1 + cospsi * costh * cosphi * x2minc2
      val dr02 = (cospsi * sinth * cosphi + sinpsi * sinphi) * x1minc1 + (cospsi * sinphi - sinpsi * sinth * cosphi) * x2minc2

      val dr10 = costh * cosphi * x0minc0 + (-sinphi * cospsi + sinpsi * sinth * cosphi) * x1minc1 + (cospsi * sinth * cosphi + sinpsi * sinphi) * x2minc2
      val dr11 = -sinth * sinphi * x0minc0 + sinpsi * costh * sinphi * x1minc1 + cospsi * costh * sinphi * x2minc2
      val dr12 = (-sinpsi * cosphi + cospsi * sinth * sinphi) * x1minc1 + (-sinpsi * sinth * sinphi - cospsi * cosphi) * x2minc2

      val dr20 = 0.0
      val dr21 = -costh * x0minc0 - sinpsi * sinth * x1minc1 - cospsi * sinth * x2minc2
      val dr22 = cospsi * costh * x1minc1 - sinpsi * costh * x2minc2

      DenseMatrix(
        (dr00, dr01, dr02),
        (dr10, dr11, dr12),
        (dr20, dr21, dr22))
    }
    df
  }
}

/** Factory for [[RotationSpace]] instances. */
object RotationSpace {
  /**
   * Factory method to create a D dimensional parametric transformation space generating rotations around the indicated centre
   *  Only _2D and _3D dimensions are supported
   */
  def apply[D : NDSpace](centre: Point[D])(implicit evCreateRot: CreateRotationSpace[D]) = {
    evCreateRot.createRotationSpace(centre)
  }

  /**
   * Factory method to create a D dimensional parametric transformation space generating rotations around the origin
   *  Only _2D and _3D dimensions are supported
   */
  def apply[D]()(implicit evDim: NDSpace[D], evCreateRot: CreateRotationSpace[D]) = {
    val origin = Point[D](DenseVector.zeros[Double](implicitly[NDSpace[D]].dimensionality).data)
    evCreateRot.createRotationSpace(origin)

  }

  private[scalismo] def eulerAnglesToRotMatrix3D(p: DenseVector[Double]): SquareMatrix[_3D] = {
    val rotMatrix = {
      // rotation matrix according to the "x-convention"
      val cospsi = Math.cos(p(2))
      val sinpsi = Math.sin(p(2))

      val costh = Math.cos(p(1))
      val sinth = Math.sin(p(1))

      val cosphi = Math.cos(p(0))
      val sinphi = Math.sin(p(0))

      SquareMatrix(
        (costh * cosphi, sinpsi * sinth * cosphi - cospsi * sinphi, sinpsi * sinphi + cospsi * sinth * cosphi),
        (costh * sinphi, cospsi * cosphi + sinpsi * sinth * sinphi, cospsi * sinth * sinphi - sinpsi * cosphi),
        (-sinth, sinpsi * costh, cospsi * costh))
    }
    rotMatrix
  }
}

/**
 * Type class required for the creation of D-dimensional Rotation spaces
 *
 */
@implicitNotFound("Rotation for dimensionality ${D} is not supported")
trait CreateRotationSpace[D] {
  def createRotationSpace(centre: Point[D]): RotationSpace[D]
}

object CreateRotationSpace {

  implicit object createRotationSpaceRotationSpace1D extends CreateRotationSpace[_1D] {
    override def createRotationSpace(centre: Point[_1D]): RotationSpace[_1D] = new RotationSpace1D
  }

  implicit object createRotationSpaceRotationSpace2D extends CreateRotationSpace[_2D] {
    override def createRotationSpace(centre: Point[_2D]): RotationSpace[_2D] = new RotationSpace2D(centre)
  }

  implicit object createRotationSpaceRotationSpace3D extends CreateRotationSpace[_3D] {
    override def createRotationSpace(centre: Point[_3D]): RotationSpace[_3D] = new RotationSpace3D(centre)
  }

}

/**
 * D-dimensional Rotation transform that is parametric, invertible and differentiable.
 */
abstract class RotationTransform[D: NDSpace] extends ParametricTransformation[D] with CanInvert[D] with CanDifferentiate[D] {
  override def inverse: RotationTransform[D]
  def center: Point[D]
}

private case class RotationTransform3D(rotMatrix: SquareMatrix[_3D], val center: Point[_3D]) extends RotationTransform[_3D] {
  override val f = (pt: Point[_3D]) => {
    val ptCentered = pt - center
    val rotCentered = rotMatrix * ptCentered
    center + EuclideanVector(rotCentered(0), rotCentered(1), rotCentered(2))
  }

  override val domain = RealSpace[_3D]

  val parameters = LandmarkRegistration.rotMatrixToEulerAngles(rotMatrix.toBreezeMatrix)

  def takeDerivative(x: Point[_3D]): SquareMatrix[_3D] = {
    rotMatrix
  }

  override def inverse: RotationTransform3D = {
    new RotationTransform3D(SquareMatrix.inv(rotMatrix), center)
  }
}

private case class RotationTransform2D(rotMatrix: SquareMatrix[_2D], val center: Point[_2D]) extends RotationTransform[_2D] {
  override val f = (pt: Point[_2D]) => {
    val ptCentered = pt - center
    val rotCentered = rotMatrix * ptCentered
    center + EuclideanVector(rotCentered(0), rotCentered(1))
  }
  override def domain = RealSpace[_2D]

  val parameters = DenseVector(LandmarkRegistration.rotationMatrixToAngle2D(rotMatrix.toBreezeMatrix))
  def takeDerivative(x: Point[_2D]): SquareMatrix[_2D] = {
    rotMatrix
  }

  override def inverse: RotationTransform2D = {
    new RotationTransform2D(SquareMatrix.inv(rotMatrix), center)
  }

}

private[scalismo] case class RotationTransform1D() extends RotationTransform[_1D] {
  override val f = (pt: Point[_1D]) => {
    pt
  }
  override val center = Point(0)

  override def domain = RealSpace[_1D]

  val parameters = DenseVector.zeros[Double](0)

  def takeDerivative(x: Point[_1D]): SquareMatrix[_1D] = throw new UnsupportedOperationException

  override def inverse: RotationTransform1D = {
    RotationTransform1D()
  }
}

/** Factory for [[RotationTransform]] instances. */
object RotationTransform {

  /**Type class required for the creation of D-dimensional Rotation transform*/
  @implicitNotFound("Rotation for dimensionality ${D} is not supported")
  trait Create[D] {
    def createRotationTransform(rotMatrix: SquareMatrix[D], centre: Point[D]): RotationTransform[D]
  }
  /** Instance of the [[RotationTransform.Create]] type class required for creating a 2-dimensional rotation space */
  implicit object createRotationTransform2D extends Create[_2D] {
    override def createRotationTransform(rotMatrix: SquareMatrix[_2D], centre: Point[_2D]): RotationTransform[_2D] = new RotationTransform2D(rotMatrix, centre)
  }
  /** Instance of the [[RotationTransform.Create]] type class required for creating a 3-dimensional rotation space */
  implicit object createRotationTransform3D extends Create[_3D] {
    override def createRotationTransform(rotMatrix: SquareMatrix[_3D], centre: Point[_3D]): RotationTransform[_3D] = new RotationTransform3D(rotMatrix, centre)
  }

  /**
   *  Factory method to create a D-dimensional rotation transform given the rotation matrix and the rotation center.
   *  Only dimensions _2D and _3D are supported.
   */
  def apply[D : NDSpace](rotMatrix: SquareMatrix[D], centre: Point[D])(implicit evCreateRot: Create[D]) = {
    evCreateRot.createRotationTransform(rotMatrix, centre)
  }

  /**
   *  Factory method to create a D-dimensional rotation transform around the origin.
   *  Only dimensions _2D and _3D are supported.
   */
  def apply[D : NDSpace](rotMatrix: SquareMatrix[D])(implicit  evCreateRot: Create[D]) = {
    val centre = Point[D](DenseVector.zeros[Double](implicitly[NDSpace[D]].dimensionality).data)
    evCreateRot.createRotationTransform(rotMatrix, centre)
  }

  /**
   * Factory method for creating a 2-dimensional rotation transform around a center, given
   * the Euler angle
   */
  def apply(phi: Double, center: Point[_2D]): RotationTransform[_2D] = {

    val rotMatrix = SquareMatrix(
      (math.cos(phi), -math.sin(phi)),
      (math.sin(phi), math.cos(phi)))

    new RotationTransform2D(rotMatrix, center)
  }

  /**
   *  Factory method to create a 3-dimensional rotation transform around a center when
   *  given the Euler angles according to the x-convention
   *
   *  @param phi rotation around the Z axis
   *  @param theta rotation around the Y axis
   *  @param psi rotation around the X axis
   *
   */
  def apply(phi: Double, theta: Double, psi: Double, centre: Point[_3D]): RotationTransform[_3D] = {
    val rotMatrix = RotationSpace.eulerAnglesToRotMatrix3D(DenseVector(phi, theta, psi))
    new RotationTransform3D(rotMatrix, centre)
  }
}

/**
 * Parametric transformation space producing isotropic scaling transforms.
 *
 */
abstract class ScalingSpace[D: NDSpace] extends TransformationSpace[D] with DifferentiableTransforms[D] {

  override type T = ScalingTransformation[D]

  def parametersDimensionality: Int = 1

  override def identityTransformParameters = DenseVector(1.0)

  override def transformForParameters(p: ParameterVector): ScalingTransformation[D] = {
    require(p.length == parametersDimensionality)
    ScalingTransformation[D](p(0))
  }
}

private class ScalingSpace3D extends ScalingSpace[_3D] {
  override def takeDerivativeWRTParameters(p: ParameterVector) = { x: Point[_3D] => DenseMatrix(x(0), x(1), x(2)) }
}

private class ScalingSpace2D extends ScalingSpace[_2D] {
  override def takeDerivativeWRTParameters(p: ParameterVector) = { x: Point[_2D] => DenseMatrix(x(0), x(1)) }
}

private class ScalingSpace1D extends ScalingSpace[_1D] {
  override def takeDerivativeWRTParameters(p: ParameterVector) = { x: Point[_1D] => DenseMatrix(x(0)) }
}
/** Factory for [[ScalingSpace]] instances.*/
object ScalingSpace {
  /**Type class required for the creation of D-dimensional Scaling space*/
  trait Create[D] {
    def createScalingSpace: ScalingSpace[D]
  }

  /** Instance of the [[ScalingSpace.Create]] type class required for creating a 1-dimensional rotation space */
  implicit object createScalingSpace1D extends Create[_1D] {
    override def createScalingSpace: ScalingSpace[_1D] = new ScalingSpace1D
  }
  /** Instance of the [[ScalingSpace.Create]] type class required for creating a 2-dimensional rotation space */
  implicit object createScalingSpace2D extends Create[_2D] {
    override def createScalingSpace: ScalingSpace[_2D] = new ScalingSpace2D
  }
  /** Instance of the [[ScalingSpace.Create]] type class required for creating a 3-dimensional rotation space */
  implicit object createScalingSpace3D extends Create[_3D] {
    override def createScalingSpace: ScalingSpace[_3D] = new ScalingSpace3D
  }
  /** returns a D-dimensional scaling space that generates isotropic scaling transforms given a scalar parameter */
  def apply[D: NDSpace]()(implicit ev: Create[D]) = ev.createScalingSpace

}

/**
 * D-dimensional isotropic scaling transform that is parametric, invertible and differentiable
 *
 *  @param s Scalar by which to scale each dimension
 */
class ScalingTransformation[D: NDSpace] private (s: Double) extends ParametricTransformation[D] with CanInvert[D] with CanDifferentiate[D] {
  override val f = (x: Point[D]) => (x.toVector * s).toPoint
  override val domain = RealSpace[D]

  val parameters = DenseVector(s)
  def takeDerivative(x: Point[D]): SquareMatrix[D] = SquareMatrix.eye[D] * s

  override def inverse: ScalingTransformation[D] = {
    if (s == 0) new ScalingTransformation[D](0) else new ScalingTransformation[D](1.0 / s)
  }
}

/** Factory for [[ScalingTransformation]] instances.*/
object ScalingTransformation {
  /**
   * Returns a D-dimensional scaling transform given the desired scaling factor.
   */
  def apply[D: NDSpace](s: Double) = new ScalingTransformation[D](s)
}

/**
 * Parametric transformation space producing rigid transforms.
 */
class RigidTransformationSpace[D: NDSpace: CreateRotationSpace] private (center: Point[D])
    extends ProductTransformationSpace[D, TranslationTransform[D], RotationTransform[D]](TranslationSpace[D], RotationSpace[D](center)) {

  def translationSpace: TransformationSpace[D] = TranslationSpace[D]
  def rotationSpace: RotationSpace[D] = RotationSpace[D](center)

  /**
   * Returns a rigid transform that rotates first then translates.
   *
   * @param p concatenated parameter vector containing first the translation parameters, then the rotaion ones
   */
  override def transformForParameters(p: ParameterVector): RigidTransformation[D] = {
    val (outerParams, innerParams) = splitProductParameterVector(p)
    new RigidTransformationTranslationFollowingRotation[D](TranslationSpace[D].transformForParameters(outerParams), RotationSpace[D](center).transformForParameters(innerParams))
  }
}
/**
 * Factory for [[RigidTransformationSpace]] instances.
 */
object RigidTransformationSpace {

  /**
   * Returns a D-dimensional Rigid Transformation Space that produces transforms that first rotate around the indicated center, then translate
   *
   */
  def apply[D: NDSpace: CreateRotationSpace](center: Point[D]) = new RigidTransformationSpace[D](center)
  def apply[D: NDSpace: CreateRotationSpace]() = {
    val origin = Point[D](DenseVector.zeros[Double](implicitly[NDSpace[D]].dimensionality).data)
    new RigidTransformationSpace[D](origin)
  }
}

/**
 * Trait for D-dimensional rigid transform, that is a composition of rotation and translation transform.
 *
 * There are different possibilities to define rigid transformations. Either we first do a translation and then a rotation,
 * or vice versa. We support both (and the inverse is always the other case).
 *
 * Instances of this trait exist only for [[_2D]] and [[_3D]] as [[RotationTransform]] is not defined for [[_1D]]
 */
trait RigidTransformation[D] extends CompositeTransformation[D] with CanInvert[D] {
  def translation: TranslationTransform[D]
  def rotation: RotationTransform[D]
  def inverse: RigidTransformation[D]

}

/** Factory for [[RigidTransformation]] instances. */
object RigidTransformation {
  /** Returns a D-dimensional rigid transform performing first a rotation then a translation*/
  def apply[D: NDSpace](translationTransform: TranslationTransform[D], rotationTransform: RotationTransform[D]): RigidTransformation[D] =
    new RigidTransformationTranslationFollowingRotation(translationTransform, rotationTransform)

  /** Returns a D-dimensional rigid transform performing first a translation then a rotation*/
  def apply[D: NDSpace](rotationTransform: RotationTransform[D], translationTransform: TranslationTransform[D]): RigidTransformation[D] =
    new RigidTransformationRotationFollowingTranslation(rotationTransform, translationTransform)

}

private class RigidTransformationTranslationFollowingRotation[D: NDSpace](val translation: TranslationTransform[D], val rotation: RotationTransform[D])
    extends CompositeTransformation[D](translation, rotation) with RigidTransformation[D] {

  def inverse = new RigidTransformationRotationFollowingTranslation[D](rotation.inverse, translation.inverse)
}

private class RigidTransformationRotationFollowingTranslation[D: NDSpace](val rotation: RotationTransform[D], val translation: TranslationTransform[D])
    extends CompositeTransformation[D](rotation, translation) with RigidTransformation[D] {
  def inverse = new RigidTransformationTranslationFollowingRotation[D](translation.inverse, rotation.inverse)

}

trait SimilarityTransformation[D] extends CompositeTransformation[D] with CanInvert[D] {
  def rigidTransformation: RigidTransformation[D]
  def scaling: ScalingTransformation[D]
  def inverse: SimilarityTransformation[D]
}

object SimilarityTransformation {
  /**
   * Constructs a similarity transformation, which first applies a rigid transformation and then
   * a scaling transformation
   *
   * @param scaling
   * @param rigidTransformation
   * @tparam D
   * @return
   */
  def apply[D: NDSpace](scaling: ScalingTransformation[D], rigidTransformation: RigidTransformation[D]): SimilarityTransformation[D] = {
    new SimilarityTransformationScalingFollowingRigid[D](scaling, rigidTransformation)
  }
  def apply[D: NDSpace](rigidTransformation: RigidTransformation[D], scaling: ScalingTransformation[D]): SimilarityTransformation[D] = {
    new SimilarityTransformationRigidFollowingScaling[D](rigidTransformation, scaling)
  }
}

private class SimilarityTransformationScalingFollowingRigid[D: NDSpace](val scaling: ScalingTransformation[D],
  val rigidTransformation: RigidTransformation[D])
    extends CompositeTransformation[D](scaling, rigidTransformation) with SimilarityTransformation[D] {

  override def inverse: SimilarityTransformation[D] = new SimilarityTransformationRigidFollowingScaling[D](rigidTransformation.inverse, scaling.inverse)
}

private class SimilarityTransformationRigidFollowingScaling[D: NDSpace](val rigidTransformation: RigidTransformation[D],
  val scaling: ScalingTransformation[D])
    extends CompositeTransformation[D](rigidTransformation, scaling) with SimilarityTransformation[D] {

  override def inverse: SimilarityTransformation[D] = new SimilarityTransformationScalingFollowingRigid(scaling.inverse, rigidTransformation.inverse)
}

/**
 * Parametric transformation space producing similarity transforms.
 *
 * @constructor Returns a parametric space generating anisotropic similarity transforms
 * @param center : center of rotation used in the rigid transform
 */
case class SimilarityTransformationSpace[D: NDSpace: CreateRotationSpace: ScalingSpace.Create](center: Point[D])
    extends ProductTransformationSpace[D, ScalingTransformation[D], RigidTransformation[D]](ScalingSpace[D](), RigidTransformationSpace[D](center)) {

  /**
   * Returns a D-dimensional similarity transform that performs the rigid transformation first, then a scaling.
   *
   * @param p parameter vector for the transform. This must be a concatenation of the scaling parameters, followed by the rigid parameters
   *
   */
  override def transformForParameters(p: ParameterVector): SimilarityTransformation[D] = {
    val (scalingP, rigidP) = splitProductParameterVector(p)
    val rigidTransformation = RigidTransformationSpace[D](center).transformForParameters(rigidP)
    SimilarityTransformation[D](ScalingTransformation[D](scalingP(0)), rigidTransformation)
  }
}

/**
 * Anisotropic scaling transform, where each dimension is scaled differently
 *
 *  @constructor creates a D-dimensional anisotropic scaling transform
 *  @param s Vector of the same dimensionality as the space indicating for each dimension the scaling factor
 */
case class AnisotropicScalingTransformation[D: NDSpace](s: geometry.EuclideanVector[D]) extends ParametricTransformation[D] with CanInvert[D] with CanDifferentiate[D] {
  override val domain = RealSpace[D]
  override val f = (x: Point[D]) => Point((x.toVector.toBreezeVector *:* s.toBreezeVector).data)

  val parameters = s.toBreezeVector
  def takeDerivative(x: Point[D]): SquareMatrix[D] = SquareMatrix[D](breeze.linalg.diag(s.toBreezeVector).data)

  override def inverse: AnisotropicScalingTransformation[D] = {
    val sinv = s.toArray.map(v => if (v == 0) 0.0 else 1.0 / v)
    new AnisotropicScalingTransformation[D](EuclideanVector[D](sinv))
  }
}
/**
 * Parametric transformation space producing anisotropic scaling transforms.
 *
 * @constructor Returns a D-dimensional anisotropic scaling space
 */
case class AnisotropicScalingSpace[D: NDSpace]() extends TransformationSpace[D] with DifferentiableTransforms[D] {
  override type T = AnisotropicScalingTransformation[D]
  def parametersDimensionality: Int = implicitly[NDSpace[D]].dimensionality

  override def identityTransformParameters = DenseVector.ones[Double](parametersDimensionality)

  /**
   * Returns a D-dimensional anisotropic scaling transform corresponding to the indicated parameters
   *
   *  @param p Scaling factor for each dimension. Must be of the same dimensionality as the space.
   */
  override def transformForParameters(p: ParameterVector): AnisotropicScalingTransformation[D] = {
    require(p.length == parametersDimensionality)
    AnisotropicScalingTransformation[D](EuclideanVector(p.data.take(parametersDimensionality)))
  }

  override def takeDerivativeWRTParameters(p: ParameterVector) = {
    x: Point[D] => new DenseMatrix(parametersDimensionality, 1, x.toArray)
  }
}
/**
 * Trait for D-dimensional anisotropic similarity transform that is a combination of a rigid transform and anisotropic scaling.
 *
 * There are different possibilities to define such a similarity transform. Either we first do a rigid transform and then scaling,
 * or vice versa. We support only one way where we scale first, then transform rigidly.
 *
 * The order of the rigid transform in this case is also fixed : first rotate then translate.
 *
 */
trait AnisotropicSimilarityTransformation[D] extends CompositeTransformation[D] with CanInvert[D] {
  override def inverse : AnisotropicSimilarityTransformation[D]
}

private class RigidTransformationThenAnisotropicScaling[D: NDSpace](anisotropicScaling: AnisotropicScalingTransformation[D], rigidTransform: RigidTransformation[D])
    extends CompositeTransformation[D](anisotropicScaling, rigidTransform) with AnisotropicSimilarityTransformation[D] {

  override def inverse: AnisotropicScalingThenRigidTransformation[D] = new AnisotropicScalingThenRigidTransformation[D](rigidTransform.inverse, anisotropicScaling.inverse)
}

private class AnisotropicScalingThenRigidTransformation[D: NDSpace](rigidTransform: RigidTransformation[D], anisotropicScaling: AnisotropicScalingTransformation[D])
    extends CompositeTransformation[D](rigidTransform, anisotropicScaling) with AnisotropicSimilarityTransformation[D] {

  override def inverse: RigidTransformationThenAnisotropicScaling[D] = new RigidTransformationThenAnisotropicScaling[D](anisotropicScaling.inverse, rigidTransform.inverse)
}

/**
 * Parametric transformation space producing anisotropic similarity transforms.
 *
 * @constructor Returns a parametric space generating anisotropic similarity transforms
 * @param center : center of rotation used in the rigid transform
 */
case class AnisotropicSimilarityTransformationSpace[D: NDSpace: CreateRotationSpace](center: Point[D])
    extends ProductTransformationSpace[D, RigidTransformation[D], AnisotropicScalingTransformation[D]](RigidTransformationSpace[D](center), AnisotropicScalingSpace[D]()) {

  /**
   * Returns a D-dimensional anisotropic similarity transform that performs scaling first, then a rigid transformation.
   *  Currently, this is the only way to create an anisotropic similarity transform as [[AnisotropicSimilarityTransformation]] does not expose
   *  constructors or factory methods.
   *
   *  The order of operations in the rigid transformation is first rotation, then translation.
   *
   *  @param p parameter vector for the transform. This must be a concatenation of the rigid transform parameters first, then scaling parameters
   *
   */
  override def transformForParameters(p: ParameterVector): AnisotropicSimilarityTransformation[D] = {
    val (rigidP, scalingP) = splitProductParameterVector(p)
    val rigid = RigidTransformationSpace[D](center).transformForParameters(rigidP).asInstanceOf[RigidTransformationTranslationFollowingRotation[D]]
    new AnisotropicScalingThenRigidTransformation[D](rigid, AnisotropicScalingSpace[D]().transformForParameters(scalingP))

  }
}
