package simulations

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class CircuitSuite extends CircuitSimulator with FunSuite {
  val InverterDelay = 1
  val AndGateDelay = 3
  val OrGateDelay = 5

  test("andGate example") {
    val in1, in2, out = new Wire
    andGate(in1, in2, out)
    in1.setSignal(false)
    in2.setSignal(false)
    run

    assert(out.getSignal === false, "and 1")

    in1.setSignal(true)
    run

    assert(out.getSignal === false, "and 2")

    in2.setSignal(true)
    run

    assert(out.getSignal === true, "and 3")
  }

  test("orGate example") {
    val in1, in2, out = new Wire
    orGate(in1, in2, out)
    in1.setSignal(false)
    in2.setSignal(false)
    run

    assert(out.getSignal === false, "or 1")

    in1.setSignal(true)
    run

    assert(out.getSignal === true, "or 2")

    in2.setSignal(true)
    run

    assert(out.getSignal === true, "or 3")
  }

  test("orGate2 example") {
    val in1, in2, out = new Wire
    orGate2(in1, in2, out)
    in1.setSignal(false)
    in2.setSignal(false)
    run

    assert(out.getSignal === false, "or 1")

    in1.setSignal(true)
    run

    assert(out.getSignal === true, "or 2")

    in2.setSignal(true)
    run

    assert(out.getSignal === true, "or 3")
  }

  test("demux example") {
    val in = new Wire
    val c1, c2 = new Wire
    val c: List[Wire] = List(c1, c2)
    val out: List[Wire] = List(new Wire, new Wire, new Wire, new Wire)

    in.setSignal(true)

    demux(in, c, out)
    run
    assert(out(0).getSignal === false, "demux 0")
    assert(out(1).getSignal === false, "demux 1")
    assert(out(2).getSignal === false, "demux 2")
    assert(out(3).getSignal === true, "demux 3")

    c1.setSignal(true)
    c2.setSignal(false)
    run

    assert(out(0).getSignal === false, "demux 0")
    assert(out(1).getSignal === false, "demux 1")
    assert(out(2).getSignal === true, "demux 2")
    assert(out(3).getSignal === false, "demux 3")

    c1.setSignal(false)
    c2.setSignal(true)
    run

    assert(out(0).getSignal === false, "demux 0")
    assert(out(1).getSignal === true, "demux 1")
    assert(out(2).getSignal === false, "demux 2")
    assert(out(3).getSignal === false, "demux 3")

    c1.setSignal(true)
    c2.setSignal(true)
    run
    assert(out(0).getSignal === true, "demux 0")
    assert(out(1).getSignal === false, "demux 1")
    assert(out(2).getSignal === false, "demux 2")
    assert(out(3).getSignal === false, "demux 3")
  }
}
