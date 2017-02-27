package suggestions


import language.postfixOps
import scala.concurrent._
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Success, Failure}
import rx.lang.scala._
import org.scalatest._
import gui._

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner


@RunWith(classOf[JUnitRunner])
class WikipediaApiTest extends FunSuite {

  object mockApi extends WikipediaApi {
    def wikipediaSuggestion(term: String) = Future {
      if (term.head.isLetter) {
        for (suffix <- List(" (Computer Scientist)", " (Footballer)")) yield term + suffix
      } else {
        List(term)
      }
    }

    def wikipediaPage(term: String) = Future {
      "Title: " + term
    }
  }

  import mockApi._

  test("WikipediaApi should make the stream valid using sanitized") {
    val notvalid = Observable("erik", "erik meijer", "martin")
    val valid = notvalid.sanitized

    var count = 0
    var completed = false

    val sub = valid.subscribe(
      term => {
        assert(term.forall(_ != ' '))
        count += 1
      },
      t => assert(false, s"stream error $t"),
      () => completed = true
    )
    assert(completed && count == 3, "completed: " + completed + ", event count: " + count)
  }

  test("WikipediaApi should correctly use concatRecovered") {
    val requests = Observable(1, 2, 3)
    val remoteComputation = (n: Int) => Observable(0 to n)
    val responses = requests concatRecovered remoteComputation
    val sum = responses.foldLeft(0) {
      (acc, tn) =>
        tn match {
          case Success(n) => acc + n
          case Failure(t) => throw t
        }
    }
    var total = -1
    val sub = sum.subscribe {
      s =>
        total = s
    }
    assert(total == (1 + 1 + 2 + 1 + 2 + 3), s"Sum: $total")
  }

  test("Correctly compose the streams that have errors using concatRecovered") {
    val requests = Observable.interval(0.25 second).timedOut(1)
    val exception = new Exception("test")
    val remoteComputation = (num: Long) => if (num != 2) Observable(num) else Observable(exception)
    val responses = requests.concatRecovered(remoteComputation)
    val actual = responses.toBlockingObservable.toList
    val expected = List(Success(0), Success(1), Failure(exception), Success(3))
    assert(actual === expected, s"actual : $actual is not as expected : $expected")
  }

  //  test("WikipediaApi should correctly use concatRecovered") {
  //    val requests = Observable(1, 2, 3)
  //    val ex = new IllegalArgumentException
  //    val remoteComputation = (n: Int) => Observable(ex)
  //    val responses = requests concatRecovered remoteComputation
  //    //    val sum = responses.foldLeft(0) {
  //    //      (acc, tn) =>
  //    //        tn match {
  //    //          case Success(n) => acc + n
  //    //          case Failure(t) => throw t
  //    //        }
  //    //    }
  //    responses.subscribe(new Observer[Try[Nothing]] {
  //      override def onError(ex: Throwable) = {
  //        assert(true)
  //      }
  //
  //      //      def asJavaObserver: rx.Observer[_ >: Int] = new rx.Observer[_] {
  //      //        def onCompleted() = {}
  //      //
  //      //        def onError(p1: Throwable) = {assert(true)}
  //      //
  //      //        def onNext(p1: _) = {}
  //      //      }
  //      def asJavaObserver: rx.Observer[_ >: Try[Nothing]] = null
  //    })
  //  }
}