package simulations


class Wire {
  private var sigVal = false
  private var actions: List[Simulator#Action] = List()

  def getSignal: Boolean = sigVal

  def setSignal(s: Boolean) {
    if (s != sigVal) {
      sigVal = s
      actions.foreach(action => action())
    }
  }

  def addAction(a: Simulator#Action) {
    actions = a :: actions
    a()
  }
}

abstract class CircuitSimulator extends Simulator {

  val InverterDelay: Int
  val AndGateDelay: Int
  val OrGateDelay: Int

  def probe(name: String, wire: Wire) {
    wire addAction {
      () => afterDelay(0) {
        println(
          "  " + currentTime + ": " + name + " -> " + wire.getSignal)
      }
    }
  }

  def inverter(input: Wire, output: Wire) {
    def invertAction() {
      val inputSig = input.getSignal
      afterDelay(InverterDelay) {
        output.setSignal(!inputSig)
      }
    }
    input addAction invertAction
  }

  def andGate(a1: Wire, a2: Wire, output: Wire) {
    def andAction() {
      val a1Sig = a1.getSignal
      val a2Sig = a2.getSignal
      afterDelay(AndGateDelay) {
        output.setSignal(a1Sig & a2Sig)
      }
    }
    a1 addAction andAction
    a2 addAction andAction
  }

  //
  // to complete with orGates and demux...
  //

  def orGate(a1: Wire, a2: Wire, output: Wire) {
    def orAction() {
      val a1Sig = a1.getSignal
      val a2Sig = a2.getSignal
      afterDelay(OrGateDelay) {
        output.setSignal(a1Sig | a2Sig)
      }
    }
    a1 addAction orAction
    a2 addAction orAction
  }

  def orGate2(a1: Wire, a2: Wire, output: Wire) {
    val notA1, notA2, notOut = new Wire
    inverter(a1, notA1)
    inverter(a2, notA2)
    andGate(notA1, notA2, notOut)
    inverter(notOut, output)
  }

  //-------------------------------------- demux --------------------------------------

  def invertedWire(inWire: Wire): Wire = {
    val outWire = new Wire;
    inverter(inWire, outWire)
    outWire
  }

  def outSet(c: List[Wire], cInv: List[Wire], binaries: List[Int]): List[Wire] = {
    if (c.isEmpty) {
      List.empty
    } else {
      (if (binaries.head == 0) c.head else cInv.head) :: outSet(c.tail, cInv.tail, binaries.tail)
    }
  }

  def toBinaryRepresentation(i: Int): List[Int] = {
    if (i == 0) {
      List.empty
    }
    else {
      (i % 2) :: toBinaryRepresentation(i / 2)
    }
  }

  def multiAnd(in: List[Wire], out: Wire) {
    for (inItem <- in) {
      andGate(inItem, out, out)
    }
  }

  def demux2(in: Wire, c: List[Wire], out: List[Wire]) {
    val cInv: List[Wire] = c.map(wIn => invertedWire(wIn))
    for (i <- 0 to c.length - 1) {
      val binaries = toBinaryRepresentation(i)
      val outWireSet = outSet(c, cInv, binaries)
      multiAnd(in :: outWireSet, out(i))
    }
  }

  def simpleDemux(in: Wire, out: Wire) {
    inverter(invertedWire(in), out)
  }

  def demux(in: Wire, c: List[Wire], out: List[Wire]) {
    if (c.isEmpty) {
      simpleDemux(in, out(0))
    }
    else {
      val head = c.head
      val headInv = invertedWire(c.head)
      val first = out.splitAt(out.length / 2)._1
      val second = out.splitAt(out.length / 2)._2

      val inAndHead = new Wire
      andGate(in, head, inAndHead)
      val inAndHeadInv = new Wire
      andGate(in, headInv, inAndHeadInv)

      demux(inAndHead, c.tail, first)
      demux(inAndHeadInv, c.tail, second)
    }
  }
}

object Circuit extends CircuitSimulator {
  val InverterDelay = 1
  val AndGateDelay = 3
  val OrGateDelay = 5

  def andGateExample {
    val in1, in2, out = new Wire
    andGate(in1, in2, out)
    probe("in1", in1)
    probe("in2", in2)
    probe("out", out)
    in1.setSignal(false)
    in2.setSignal(false)
    run

    in1.setSignal(true)
    run

    in2.setSignal(true)
    run
  }

  //
  // to complete with orGateExample and demuxExample...
  //
}

object CircuitMain extends App {
  // You can write tests either here, or better in the test class CircuitSuite.
  Circuit.andGateExample
}
