import Exploration.*

import scala.collection.mutable.ListBuffer

enum Maze:
  case Branch(label: String, left: Maze, right: Maze, var status: Exploration = UnExplored)
  case Leaf(label: String)

  def explore: List[String] = this match
    case branch@Branch(label, left, right, status) =>
      status match
        case UnExplored => branch.status = Explored; label :: left.explore ::: right.explore
        case Explored => List(label)
    case Leaf(label) => List(label)

  def explore2(trace: ListBuffer[String]): Unit = this match
    case branch@Branch(label, left, right, status) =>
      status match
        case UnExplored => branch.status = Explored; trace += label; left.explore2(trace); right.explore2(trace)
        case Explored => trace += label
    case Leaf(label) => trace += label