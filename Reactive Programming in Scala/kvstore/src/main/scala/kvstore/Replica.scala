package kvstore

import akka.actor.{Props, ActorRef, Actor}
import kvstore.Arbiter._
import kvstore.Replica._
import kvstore.Replica.Remove
import kvstore.Replica.Get
import kvstore.Replica.GetResult
import kvstore.Replica.Insert
import kvstore.Replicator.{Replicate, SnapshotAck, Snapshot}
import scala.concurrent.duration._

object Replica {

  sealed trait Operation {
    def key: String

    def id: Long
  }

  case class Insert(key: String, value: String, id: Long) extends Operation

  case class Remove(key: String, id: Long) extends Operation

  case class Get(key: String, id: Long) extends Operation

  sealed trait OperationReply

  case class OperationAck(id: Long) extends OperationReply

  case class OperationFailed(id: Long) extends OperationReply

  case class GetResult(key: String, valueOption: Option[String], id: Long) extends OperationReply

  def props(arbiter: ActorRef, persistenceProps: Props): Props = Props(new Replica(arbiter, persistenceProps))
}

class Replica(val arbiter: ActorRef, persistenceProps: Props) extends Actor {


  /*
   * The contents of this actor is just a suggestion, you can implement it in any way you like.
   */

  var kv = Map.empty[String, String]
  // a map from secondary replicas to replicators
  var secondaries = Map.empty[ActorRef, ActorRef]
  // the current set of replicators
  var replicators = Set.empty[ActorRef]

  override def preStart(): Unit = {
    arbiter ! Join // TODO
  }

  def receive = {
    case JoinedPrimary => context.become(leader)
    case JoinedSecondary => context.become(replica)
  }

  var _seqCounter = 0L

  def nextSeq = {
    val ret = _seqCounter
    _seqCounter += 1
    ret
  }

  /* TODO Behavior for  the leader role. */
  val leader: Receive = {
    case Replicas(replicas) =>
      val secondariesAdded: Set[ActorRef] = (replicas -- secondaries.keySet) - self
      // TODO: can be more that one?

      val replicator = context.actorOf(Replicator.props(secondariesAdded.head))
      secondaries = secondaries.updated(secondariesAdded.head, replicator)
      // TODO: retry
      kv.foreach(entry => replicator ! Replicate(entry._1, Some(entry._2), nextSeq))

      // TODO: secondary removed
    case Get(key, id) => sender ! GetResult(key, kv.get(key), id)
    case Insert(key, value, id) =>
      kv = kv.updated(key, value)
      replicators.foreach(it => it ! Replicate(key, Some(value), id)) // TODO: new id?
      sender ! OperationAck(id)
    case Remove(key, id) =>
      kv -= key
      replicators.foreach(it => it ! Replicate(key, None, id)) // TODO: new id?
      sender ! OperationAck(id)
  }

  var next = 0

//  context.setReceiveTimeout(100 milliseconds)
  //  override def supervisorStrategy() = OneForOneStrategy(maxNrOfRetries = 2) {
  //    case _: IOException => Restart
  //  }

  /* TODO Behavior for the replica role. */
  val replica: Receive = {
    case Get(key, id) => sender ! GetResult(key, kv.get(key), id)
    case Snapshot(key, valueOption, seq) =>
      if (seq < next) {
        sender ! SnapshotAck(key, seq)
      }
      else if (seq > next) {
        // is ignored
      }
      else {
        // seq == next
        valueOption match {
          case Some(value) => kv = kv.updated(key, value)
          case None => kv -= key
        }
        sender ! SnapshotAck(key, seq)
        next = next + 1
      }
  }
}
