package scrapper

trait TreeNode
class Tree(startTag: String, child: List[TreeNode], endTag: String, level: Int) extends TreeNode {
  override def toString =
    s"""${"\t"*level}${startTag}
       |${child.mkString("\n")}
       |${"\t"*level}${endTag}
       |""".stripMargin
}

class TextNode(text: String, level: Int) extends TreeNode {
  override def toString: String = s"${"\t"*level}${text}"
}


object Scrapper {

  private def getTreeList(str: String, ls: List[TreeNode], level: Int): (List[TreeNode], String) = {
    val newStr = str.trim
    if(newStr.startsWith("</"))
      (ls, newStr)
    else {
      val (node, rest) = getTree(newStr, level)
      getTreeList(rest, ls ++ List(node), level)
    }
  }
  def getTree(str: String, level: Int = 0) : (TreeNode, String)  = {
    val newText = str.trim
    if(newText.isEmpty)
      (new TextNode("", level), "")
    else
      newText.head match {
        case '<' => val (startTag, rest)  = newText.splitAt(newText.indexOf('>')+1)
          val (child, rest2) = getTreeList(rest, List.empty, level+1)
          val (endTag, rest3) = rest2.trim.splitAt(rest2.trim.indexOf('>')+1)
          (new Tree(startTag, child, endTag, level), rest3)
        case _ => val (text, rest)  = newText.splitAt(newText.indexOf('<'))
          (new TextNode(text, level), rest)
      }
  }

}
