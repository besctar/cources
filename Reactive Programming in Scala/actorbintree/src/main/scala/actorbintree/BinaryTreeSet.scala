/**
 * Copyright (C) 2009-2013 Typesafe Inc. <http://www.typesafe.com>
 */
package actorbintree

import akka.actor._
import scala.collection.immutable.Queue

object BinaryTreeSet {

  trait Operation {
    def requester: ActorRef

    def id: Int

    def elem: Int
  }

  trait OperationReply {
    def id: Int
  }

  /** Request with identifier `id` to insert an element `elem` into the tree.
    * The actor at reference `requester` should be notified when this operation
    * is completed.
    */
  case class Insert(requester: ActorRef, id: Int, elem: Int) extends Operation

  /** Request with identifier `id` to check whether an element `elem` is present
    * in the tree. The actor at reference `requester` should be notified when
    * this operation is completed.
    */
  case class Contains(requester: ActorRef, id: Int, elem: Int) extends Operation

  /** Request with identifier `id` to remove the element `elem` from the tree.
    * The actor at reference `requester` should be notified when this operation
    * is completed.
    */
  case class Remove(requester: ActorRef, id: Int, elem: Int) extends Operation

  /** Request to perform garbage collection */
  case object GC

  /** Holds the answer to the Contains request with identifier `id`.
    * `result` is true if and only if the element is present in the tree.
    */
  case class ContainsResult(id: Int, result: Boolean) extends OperationReply

  /** Message to signal successful completion of an insert or remove operation. */
  case class OperationFinished(id: Int) extends OperationReply

}


class BinaryTreeSet extends Actor {

  import BinaryTreeSet._

  def createRoot: ActorRef = context.actorOf(BinaryTreeNode.props(0, initiallyRemoved = true))

  var root = createRoot

  //  var gcInAction = false

  var pendingQueue = Queue.empty[Operation]

  def receive = normal

  /** Accepts `Operation` and `GC` messages. */
  val normal: Receive = {
    case Insert(requester, id, elem) => {
      root ! Insert(requester, id, elem)
    }
    case Contains(requester, id, elem) => {
      root ! Contains(requester, id, elem)
    }
    case Remove(requester, id, elem) => {
      root ! Remove(requester, id, elem)
    }
    case GC => {
      val newRoot = createRoot
      root ! BinaryTreeNode.CopyTo(newRoot)
      context.become(garbageCollecting(createRoot))
    }
  }

  /** Handles messages while garbage collection is performed.
    * `newRoot` is the root of the new binary tree where we want to copy
    * all non-removed elements into.
    */
  def garbageCollecting(newRoot: ActorRef): Receive = {
    case GC => ()
    case BinaryTreeNode.CopyFinished => {
      root = newRoot
      context.unbecome()
      pendingQueue.iterator.foreach(op => root ! op)
    }
    case _ => pendingQueue.enqueue(_)
  }

}

object BinaryTreeNode {

  trait Position

  case object Left extends Position

  case object Right extends Position

  case class CopyTo(treeNode: ActorRef)

  case object CopyFinished

  def props(elem: Int, initiallyRemoved: Boolean) = Props(classOf[BinaryTreeNode], elem, initiallyRemoved)
}

class BinaryTreeNode(val elem: Int, initiallyRemoved: Boolean) extends Actor {

  import BinaryTreeNode._
  import BinaryTreeSet._

  var subtrees = Map[Position, ActorRef]()
  var removed = initiallyRemoved

  def receive = normal

  /** Handles `Operation` messages and `CopyTo` requests. */
  val normal: Receive = {
    case CopyTo(treeNode) => {
      if (!removed) {
        treeNode ! Insert(self, elem, elem)
      }
      if (subtrees.contains(Left))
        subtrees(Left) ! CopyTo(treeNode)
      if (subtrees.contains(Right))
        subtrees(Right) ! CopyTo(treeNode)
      sender ! CopyFinished
      context.become(copying(subtrees.values.toSet, removed))
    }
    case Insert(requester, id, elem) => {
      if (elem == this.elem) {
        removed = false
        requester ! OperationFinished(id)
      } else if (elem > this.elem && subtrees.contains(Right)) {
        subtrees(Right) ! Insert(requester, id, elem)
      } else if (elem > this.elem && !subtrees.contains(Right)) {
        subtrees = subtrees.updated(Right, context.actorOf(BinaryTreeNode.props(elem, false)))
        requester ! OperationFinished(id)
      } else if (elem < this.elem && subtrees.contains(Left)) {
        subtrees(Left) ! Insert(requester, id, elem)
      } else if (elem < this.elem && !subtrees.contains(Left)) {
        subtrees = subtrees.updated(Left, context.actorOf(BinaryTreeNode.props(elem, false)))
        requester ! OperationFinished(id)
      }
    }
    case Contains(requester, id, elem) => {
      if (elem == this.elem) {
        requester ! ContainsResult(id, !removed)
      } else if (elem > this.elem && subtrees.contains(Right)) {
        subtrees(Right) ! Contains(requester, id, elem)
      } else if (elem < this.elem && subtrees.contains(Left)) {
        subtrees(Left) ! Contains(requester, id, elem)
      } else {
        requester ! ContainsResult(id, false)
      }
    }
    case Remove(requester, id, elem) => {
      if (elem == this.elem) {
        removed = true
        requester ! OperationFinished(id)
      } else if (elem < this.elem && subtrees.contains(Left)) {
        subtrees(Left) ! Remove(requester, id, elem)
      } else if (elem > this.elem && subtrees.contains(Right)) {
        subtrees(Right) ! Remove(requester, id, elem)
      } else {
        requester ! OperationFinished(id)
      }
    }
  }

  /** `expected` is the set of ActorRefs whose replies we are waiting for,
    * `insertConfirmed` tracks whether the copy of this node to the new tree has been confirmed.
    */
  def copying(expected: Set[ActorRef], insertConfirmed: Boolean): Receive = {
    case OperationFinished => {

    }
    case CopyFinished =>
      context.unbecome()
    case CopyTo(treeNode) => {
      if (!removed) {
        treeNode ! Insert(context.parent, elem, elem)
      }
      if (subtrees.contains(Left))
        subtrees(Left) ! CopyTo(treeNode)
      if (subtrees.contains(Right))
        subtrees(Right) ! CopyTo(treeNode)
      context.stop(self)
    }
  }
}