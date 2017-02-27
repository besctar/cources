import scala.AnyVal
import scala.language.postfixOps
import scala.util._
import scala.concurrent._
import scala.concurrent.duration._
import ExecutionContext.Implicits.global

/** Contains basic data types, data structures and `Future` extensions.
  */
package object nodescala {

  /** Adds extensions methods to the `Future` companion object.
    */
  implicit class FutureCompanionOps[T](val f: Future.type) extends AnyVal {

    /** Returns a future that is always completed with `value`.
      */
    def always[T](value: T): Future[T] = {
      val p = promise[T]
      p.success(value)
      p.future
    }

    /** Returns a future that is never completed.
      *
      * This future may be useful when testing if timeout logic works correctly.
      */
    def never[T]: Future[T] = promise[T].future

    /** Given a list of futures `fs`, returns the future holding the list of values of all the futures from `fs`.
      * The returned future is completed only once all of the futures in `fs` have been completed.
      * The values in the list are in the same order as corresponding futures `fs`.
      * If any of the futures `fs` fails, the resulting future also fails.
      */
    def all[T](fs: List[Future[T]]): Future[List[T]] = f.sequence(fs)

    //      future {
    //        fs.foldRight(List[T]())((fsItem, list) => {
    //          Await.result(fsItem, 1 second) :: list
    //        })
    //      }

    //    {
    //      val p = promise[List[T]]
    //      @volatile var fsItems = List[T]()
    //      for (fsItem <- fs)
    //        fsItem.onComplete(item => {
    //          fsItems = item.get :: fsItems
    //          if (fsItems.length == fs.length) {
    //            p.complete(Try(fsItems))
    //          }
    //        })
    //      p.future
    //    }

    /** Given a list of futures `fs`, returns the future holding the value of the future from `fs` that completed first.
      * If the first completing future in `fs` fails, then the result is failed as well.
      *
      * E.g.:
      *
      * Future.any(List(Future { 1 }, Future { 2 }, Future { throw new Exception }))
      *
      * may return a `Future` succeeded with `1`, `2` or failed with an `Exception`.
      */
    def any[T](fs: List[Future[T]]): Future[T] = {
      val p = promise[T]
      for (fsItem <- fs) fsItem.onComplete(it => p.tryComplete(it))
      p.future
    }

    /** Returns a future with a unit value that is completed after time `t`.
      */
    def delay(t: Duration): Future[Unit] = future {
      blocking {
        Thread.sleep(t.toMillis)
      }
    }

    /** Completes this future with user input.
      */
    def userInput(message: String): Future[String] = future {
      readLine(message)
    }

    /** Creates a cancellable context for an execution and runs it.
      */
    def run()(f: CancellationToken => Future[Unit]): Subscription = {
      val tokenSource = CancellationTokenSource()
      f(tokenSource.cancellationToken)
      tokenSource
    }
  }

  /** Adds extension methods to future objects.
    */
  implicit class FutureOps[T](val f: Future[T]) extends AnyVal {

    /** Returns the result of this future if it is completed now.
      * Otherwise, throws a `NoSuchElementException`.
      *
      * Note: This method does not wait for the result.
      * It is thus non-blocking.
      * However, it is also non-deterministic -- it may throw or return a value
      * depending on the current state of the `Future`.
      */
    def now: T = f.value.get.get

    /** Continues the computation of this future by taking the current future
      * and mapping it into another future.
      *
      * The function `cont` is called only after the current future completes.
      * The resulting future contains a value returned by `cont`.
      */
    def continueWith[S](cont: Future[T] => S): Future[S] =
      f.flatMap(it => future {
        cont(f)
      })

    //      future {}
    //        cont(f)
    //      }

    /** Continues the computation of this future by taking the result
      * of the current future and mapping it into another future.
      *
      * The function `cont` is called only after the current future completes.
      * The resulting future contains a value returned by `cont`.
      */
    def continue[S](cont: Try[T] => S): Future[S] = {
      val p = promise[S]
      f.onComplete(it => p.complete(Try(cont(it))))
      p.future
    }
  }

  /** Subscription objects are used to be able to unsubscribe
    * from some event source.
    */
  trait Subscription {
    def unsubscribe(): Unit
  }

  object Subscription {
    /** Given two subscriptions `s1` and `s2` returns a new composite subscription
      * such that when the new composite subscription cancels both `s1` and `s2`
      * when `unsubscribe` is called.
      */
    def apply(s1: Subscription, s2: Subscription) = new Subscription {
      def unsubscribe() {
        s1.unsubscribe()
        s2.unsubscribe()
      }
    }
  }

  /** Used to check if cancellation was requested.
    */
  trait CancellationToken {
    def isCancelled: Boolean

    def nonCancelled = !isCancelled
  }

  /** The `CancellationTokenSource` is a special kind of `Subscription` that
    * returns a `cancellationToken` which is cancelled by calling `unsubscribe`.
    *
    * After calling `unsubscribe` once, the associated `cancellationToken` will
    * forever remain cancelled -- its `isCancelled` will return `false.
    */
  trait CancellationTokenSource extends Subscription {
    def cancellationToken: CancellationToken
  }

  /** Creates cancellation token sources.
    */
//  object CancellationTokenSource {
//    /** Creates a new `CancellationTokenSource`.
//      */
//    def apply(): CancellationTokenSource = new CancellationTokenSource {
//      val p = promise[Unit]
//
//      def unsubscribe = p.trySuccess()
//
//      def cancellationToken: CancellationToken = new CancellationToken {
//        def isCancelled = p.isCompleted
//      }
//    }
//  }

  object CancellationTokenSource {
    def apply(): CancellationTokenSource = new CancellationTokenSource {
      val p = Promise[Unit]()
      val cancellationToken = new CancellationToken {
        def isCancelled = p.future.value != None
      }
      def unsubscribe() {
        p.trySuccess(())
      }
    }
  }

}

