package net.aethersanctum.practice

import org.junit.Test
import org.junit.Assert._
import org.apache.commons.logging.Log
import org.apache.commons.logging.LogFactory

class IBSTTest {
  val LOG = LogFactory.getLog(getClass)

  @Test
  def testTree {
    val empty = new ImmutableBinarySearchTree[String]()

    val words = Array("kelp", "sand", "fish", "shark", "squid", "jellyfish")

    /* starting with an empty tree, fold over the words, producing a modified copy of
     * the tree with each word inserted.
     */
    val tree = words.foldLeft(empty) {
      _ insert _
    }

    // inorder traversal
    tree.foreach(item => LOG.info(item))

    // append each item to the tree to a stringbuilder, in order. should result in alphabetical order
    val combined = tree.foldLeft(new StringBuilder) {
      (buf, item) => buf append item append " "
    }.toString

    LOG.info("inorder traversal results in: " + combined)
    assertEquals("fish jellyfish kelp sand shark squid ", combined)
  }

}