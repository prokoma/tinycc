package tinycc.common.analysis

import org.scalatest.funsuite.AnyFunSuite
import tinycc.common.Cfg

import scala.collection.mutable

class LoopAnalysisTest extends AnyFunSuite {

  class MockCfg(_entryNodes: Int, _exitNodes: Seq[Int], _adjList: Map[Int, Seq[Int]]) extends Cfg[Int] {
    private val _invAdjList = mutable.Map.empty[Int, List[Int]].withDefaultValue(List.empty)
    for ((u, adj) <- _adjList; v <- adj) _invAdjList(v) ::= u

    override val nodes: Seq[Int] = _adjList.keys.toSeq

    override def entryNodes: Seq[Int] = Seq(_entryNodes)

    override def exitNodes: Seq[Int] = _exitNodes

    override def getSucc(node: Int): Seq[Int] = _adjList(node)

    override def getPred(node: Int): Seq[Int] = _invAdjList(node)
  }

  test("linear graph") {
    val cfg = new MockCfg(1, Seq(4), Seq(
      1 -> Seq(2),
      2 -> Seq(3),
      3 -> Seq(4),
      4 -> Seq(),
    ).toMap)

    val nodesInLoop = new LoopAnalysis(cfg).result()
    assert(nodesInLoop == Set.empty[Int])
  }

  test("graph with loop") {
    val cfg = new MockCfg(1, Seq(5), Seq(
      1 -> Seq(2, 3),
      2 -> Seq(3),
      3 -> Seq(4, 5),
      4 -> Seq(2),
      5 -> Seq()
    ).toMap)

    val nodesInLoop = new LoopAnalysis(cfg).result()
    assert(nodesInLoop == Set(2, 3, 4))
  }

  test("graph with regular and nested loop") {
    val cfg = new MockCfg(1, Seq(6, 10), Seq(
      1 -> Seq(2),
      2 -> Seq(2, 3, 7),
      3 -> Seq(4),
      4 -> Seq(5),
      5 -> Seq(3, 6),
      6 -> Seq(),
      7 -> Seq(9),
      8 -> Seq(10, 7),
      9 -> Seq(9, 8),
      10 -> Seq(),
    ).toMap)

    val nodesInLoop = new LoopAnalysis(cfg).result()
    assert(nodesInLoop == Set(2, 3, 4, 5, 7, 8, 9))
  }

}
