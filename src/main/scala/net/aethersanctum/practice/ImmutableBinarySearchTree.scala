package net.aethersanctum.practice

case class TreeNode[T <: Comparable[T]](value: T,
                                        left: Option[TreeNode[T]] = None,
                                        right: Option[TreeNode[T]] = None) {

  /**
   * create a copy of this treenode with the item inserted as a child of it
   */
  def insert(item: TreeNode[T]): TreeNode[T] = {

    // creates a replacement subtree for left or right child
    def replacement(child: Option[TreeNode[T]]) = {
      child map {
        _ insert item // if child is present, delegate to it
      } orElse {
        Some(item) // otherwise can use item as child
      }
    }
    // do we need to replace left or right subtree?
    val cmp = item.value compareTo this.value
    // return new copy of this with appropriate subtree replaced
    if (cmp < 0) {
      this.copy(left = replacement(left))
    }
    else {
      this.copy(right = replacement(right))
    }
  }

  /**
   * inorder iteration over each element in the tree
   */
  def foreach(todo: T => Unit) {
    left map {
      _ foreach todo
    }
    todo(value)
    right map {
      _ foreach todo
    }
  }

  /**
   * return true if the item is in the tree
   */
  def contains(item: T): Boolean = {
    val cmp = item compareTo value
    if (cmp == 0) {
      true
    } else {
      val next = (if (cmp < 0) left else right)
      next map {
        _ contains item
      } getOrElse false
    }
  }

  override def toString = {
    val leftStr = left map { _.value } getOrElse "?"
    val rightStr = right map { _.value } getOrElse "?"
    "TreeNode[value=" + value +
            ",left=" + leftStr +
            ",right=" + rightStr + "]"
  }
}

/**
 * Container class for the root node
 */
case class ImmutableBinarySearchTree[T <: Comparable[T]](root: Option[TreeNode[T]] = None) {

  def insert(item: T) = {
    val newNode = TreeNode(item)
    ImmutableBinarySearchTree(
      root map {
        _ insert newNode // delegate to existing root
      } orElse {
        Some(newNode) // become initial root
      }
    )
  }

  def contains(item: T): Boolean = {
    root map {
      _ contains item
    } getOrElse false
  }

  def foreach(todo: T => Unit) {
    root map {
      _ foreach todo
    }
  }

  def foldLeft[B](init: B)(accumulate: (B, T) => B) = {
    var result = init // mutable but not shared, so not entirely evil
    root map {
      _ foreach {
        item =>
          result = accumulate(result, item)
      }
    }
    result
  }

}