package nop.frontend
package support

private final val INDENT: String = "  "

private def mkOffset(depth: Int) = INDENT * depth

class Tree(val item: Any, val children: List[Tree]):
  override def toString: String = toString(0)

  def toString(depth: Int): String =
    mkOffset(depth)
      + item
      + '\n'
      + children.map(_.toString(depth + 1)).mkString

trait PrettyPrintable:
  def toTree: Tree
