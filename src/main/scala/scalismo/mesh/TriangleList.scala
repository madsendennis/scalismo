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
package scalismo.mesh

import scalismo.common.PointId

/** express a triangulation, contains only triangle information, no points */
case class TriangleList(triangles: IndexedSeq[TriangleCell]) {

  /** get a specific triangle */
  def triangle(id: TriangleId): TriangleCell = triangles(id.id)

  private val pointIdAsIntRange = extractRange(triangles)

  /** list of all PointIds referenced by this triangulation (always a *range*, even if it has holes) */
  lazy val pointIds: IndexedSeq[PointId] = pointIdAsIntRange.map { PointId }

  /***/
  lazy val triangleIds: IndexedSeq[TriangleId] = triangles.indices.map(i => TriangleId(i))

  /** points adjacent to a point */
  def adjacentPointsForPointData(pointId: PointId): IndexedSeq[PointId] = accessPointData(adjacentPointsForPointData, pointId)

  /** triangles adjacent to a point */
  def adjacentTrianglesForPoint(pointId: PointId): IndexedSeq[TriangleId] = accessPointData(adjacentTrianglesForPointData, pointId)

  /** points adjacent to a triangle */
  def adjacentPointsForTriangle(triangleId: TriangleId): IndexedSeq[PointId] = accessTriangleData(adjacentPointsForTriangleData, triangleId)

  /** triangles adjacent to a triangle */
  def adjacentTrianglesForTriangle(triangleId: TriangleId): IndexedSeq[TriangleId] = accessTriangleData(adjacentTrianglesForTriangleData, triangleId)

  /**
   * safe point data accessor: check availability and maps to proper data index (using storage starting with index 0)
   * returns an empty data list for invalid PointId, this occurs for meshes with more points than referenced in this triangulation
   */
  private def accessPointData[A](data: IndexedSeq[IndexedSeq[A]], pointId: PointId): IndexedSeq[A] = {
    if (pointIdAsIntRange.isDefinedAt(pointId.id))
      data(pointId.id - pointIdAsIntRange.min)
    else
      IndexedSeq.empty[A]
  }

  /**
   * safe triangle data accessor: check availability
   * fails for invalid TriangleIds
   */
  private def accessTriangleData[A](data: IndexedSeq[IndexedSeq[A]], triangleId: TriangleId): IndexedSeq[A] = {
    if (triangles.indices.isDefinedAt(triangleId.id))
      data(triangleId.id)
    else
      throw new RuntimeException("Invalid TriangleId: " + triangleId)
  }

  /** cached data for triangles around a point */
  private lazy val adjacentTrianglesForPointData: IndexedSeq[IndexedSeq[TriangleId]] = {
    // list structure
    val emptyMapData = for (p <- pointIds) yield p -> collection.mutable.Set.empty[TriangleId]
    val triangleMap = emptyMapData.toMap

    for (t <- triangleIds) {
      val triangle = triangles(t.id)
      triangleMap(triangle.ptId1) += t
      triangleMap(triangle.ptId2) += t
      triangleMap(triangle.ptId3) += t
    }
    val data: Map[PointId, Set[TriangleId]] = triangleMap.mapValues(s => s.toSet) // make immutable
    // make available per point id, via fast random access in IndexedSeq, starts at 0
    IndexedSeq.tabulate(pointIds.size) { i => data(pointIds(i)).toIndexedSeq }
  }

  /** points adjacent to a point: cached data structure */
  private lazy val adjacentPointsForPointData: IndexedSeq[IndexedSeq[PointId]] = {
    // all co-occurrences in triangle list: all points reachable by a link
    val emptyMapData = for (p <- pointIds) yield p -> collection.mutable.Set.empty[PointId]
    val pointMap = emptyMapData.toMap

    for (t <- triangleIds) {
      val triangle = triangles(t.id)
      pointMap(triangle.ptId1) ++= triangle.pointIds
      pointMap(triangle.ptId2) ++= triangle.pointIds
      pointMap(triangle.ptId3) ++= triangle.pointIds
    }
    for (p <- pointIds) {
      pointMap(p) -= p
    }
    val mapData: Map[PointId, Set[PointId]] = pointMap.mapValues(s => s.toSet) // make immutable
    // fast random access via PointId, starts at 0, use pointDataIndex(pointId) to access
    IndexedSeq.tabulate(pointIds.size) { i => mapData(pointIds(i)).toIndexedSeq }
  }

  /** triangles connected to triangle via common point */
  private lazy val adjacentTrianglesForTriangleData: IndexedSeq[IndexedSeq[TriangleId]] = {
    // for each triangle get the 3 defining vertices, for each of those get all surrounding triangles, remove self
    val emptyMapData = for (t <- triangleIds) yield t -> collection.mutable.Set.empty[TriangleId]
    val triangleMap = emptyMapData.toMap

    for (t <- triangleIds) {
      triangleMap(t) ++= triangles(t.id).pointIds.flatMap(p => adjacentTrianglesForPoint(p))
      triangleMap(t) -= t
    }
    val mapData: Map[TriangleId, Set[TriangleId]] = triangleMap.mapValues(s => s.toSet)
    IndexedSeq.tabulate(triangleIds.size) { i => mapData(triangleIds(i)).toIndexedSeq }
  }

  /** points connected to a triangle, this information is contained in triangles */
  private lazy val adjacentPointsForTriangleData: IndexedSeq[IndexedSeq[PointId]] = triangles.map { _.pointIds }

  /** extract the range of PointIds referenced by this triangulation (assumes a range) */
  private[this] def extractRange(triangles: IndexedSeq[TriangleCell]): IndexedSeq[Int] = {
    if (triangles.isEmpty) {
      IndexedSeq.empty[Int]
    } else {
      val min = triangles.flatMap(t => t.pointIds).minBy(_.id)
      val max = triangles.flatMap(t => t.pointIds).maxBy(_.id)
      min.id to max.id
    }
  }
}

/** Id of a triangle in a TriangleList */
final case class TriangleId(id: Int) extends AnyVal {
  def isValid: Boolean = this != TriangleId.invalid
}

object TriangleId {
  /** an invalid TriangleId */
  val invalid = TriangleId(-1)
}

object TriangleList {
  val empty = TriangleList(IndexedSeq.empty[TriangleCell])
}
