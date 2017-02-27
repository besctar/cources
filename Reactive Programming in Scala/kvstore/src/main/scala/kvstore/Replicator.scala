package kvstore

import akka.actor._
import akka.actor.SupervisorStrategy.Restart
import java.io.IOException
import scala.concurrent.duration._

object Replicator {

  case class Replicate(key: String, valueOption: Option[String], id: Long)

  case class Replicated(key: String, id: Long)

  case class Snapshot(key: String, valueOption: Option[String], seq: Long)

  case class SnapshotAck(key: String, seq: Long)

  def props(replica: ActorRef): Props = Props(new Replicator(replica))
}

class Replicator(val replica: ActorRef) extends Actor {

  import Replicator._

  /*
   * The contents of this actor is just a suggestion, you can implement it in any way you like.
   */

  // map from sequence number to pair of sender and request
  var acks = Map.empty[Long, (ActorRef, Replicate)]
  // a sequence of not-yet-sent snapshots (you can disregard this if not implementing batching)
  var pending = Vector.empty[Snapshot]

  var _seqCounter = 0L
  def nextSeq = {
    val ret = _seqCounter
    _seqCounter += 1
    ret
  }

  context.setReceiveTimeout(100 milliseconds)
  context.system.scheduler.scheduleOnce(100 milliseconds)(  f)
//  override def supervisorStrategy() = OneForOneStrategy(maxNrOfRetries = 2) {
//    case _: IOException => Restart
//  }

  /* TODO Behavior for the Replicator. */
  def receive: Receive = {
    case ReceiveTimeout =>
     acks.head
    case Replicate(key, valueOption, id) =>
      val next = nextSeq
      acks = acks.updated(next, (sender, Replicate(key, valueOption, id)))
      // pending???
      replica ! Snapshot(key, valueOption, next)
    case SnapshotAck(key, seq) =>
      val pair = acks(seq)
      val ref = pair._1
      val replicate = pair._2
      ref ! Replicated(replicate.key, replicate.id)
      acks -= seq
  }
}
