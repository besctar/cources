package simulations

import math.random

class EpidemySimulator extends Simulator {

  def randomBelow(i: Int) = (random * i).toInt

  protected[simulations] object SimConfig {
    val day: Int = 1;
    val population: Int = 300
    val roomRows: Int = 8
    val roomColumns: Int = 8
    val prevalenceRate: Int = 1
    val transmissibilityRate: Int = 40
    val deathRate = 25

    val moveTime: Int = 5

    val sickPeriod: Int = 6
    val diePeriod: Int = 14
    val immunePeriod: Int = 16
    val healthyPeriod: Int = 18
  }

  import SimConfig._

  def prevalenceCount: Int = (prevalenceRate * population) / 100

  def probability(rate: Int): Boolean = randomBelow(100) < rate

  def person(id: Int, infected: Boolean): Person = {
    val person: Person = new Person(id)
    if (infected) {
      person.infected = infected
      person.infectionTime = currentTime
    }
    person.scheduleMove()
    person
  }

  val persons: List[Person] = (0 to population).map(i => if (i < prevalenceCount) person(i, true) else person(i, false)).toList

  {
    scheduleLifecycle
  }

  case class Position(val row: Int, val col: Int) {
    def neighbours(): List[Position] =
      List(Position(row + 1, col), Position(row - 1, col), Position(row, col + 1), Position(row, col - 1))
        .map(p =>
        if (p.row < 0) Position(roomRows - 1, col)
        else if (p.row > roomRows - 1) Position(0, col)
        else if (p.col < 0) Position(row, roomColumns - 1)
        else if (p.col > roomColumns - 1) Position(row, 0)
        else p)

    def near(p: Position): Boolean = neighbours().contains(p)
  }


  class Person(val id: Int) {
    var infected = false
    var sick = false
    var immune = false
    var dead = false

    var deathProbabilityHappened = false

    var infectionTime: Int = -1000

    var row: Int = randomBelow(roomRows)
    var col: Int = randomBelow(roomColumns)

    def position(): Position = Position(row, col)

    def nextMove(): Position = {
      val neighbours: List[Position] = position().neighbours()
      val direction: Int = randomBelow(neighbours.size)
      neighbours(direction)
    }

    def time(period: Int): Boolean = {
      currentTime - infectionTime >= period
    }

    def inSickPeriod(): Boolean = time(sickPeriod)

    def inDiePeriod(): Boolean = time(diePeriod)

    def inImmunePeriod(): Boolean = time(immunePeriod)

    def inHealthyPeriod(): Boolean = time(healthyPeriod)

    def moveAction() {
      if (dead) {
        return
      }
      val neighbours: List[Position] = this.position.neighbours
      val neighbourRoomsFine: Boolean = persons.filter(person => neighbours.contains(person.position)).forall(person => (!person.sick && !person.dead))

      persons.flatMap
      if (neighbourRoomsFine) {
        val possibleMoves: List[Position] = neighbours.filterNot(pos => persons.exists(person => (person.position == pos) && (person.sick || person.dead)))
        val move: Position = possibleMoves(randomBelow(possibleMoves.size))
        row = move.row
        col = move.col
      }
      scheduleMove()
    }

    def scheduleMove() {
      afterDelay(randomBelow(moveTime))(moveAction)
    }
  }

  def lifecycle {
    persons.foreach(p =>
      if (p.dead) {
        return
      } else if (!p.infected && !p.sick && !p.dead) {
        val infectedInTheRoom = persons.filter(other => (other != p) && (other.position == p.position)).exists(some => some.infected || some.immune || some.sick || some.dead)
        if (infectedInTheRoom) {
          val willBeInfected = probability(transmissibilityRate)
          if (willBeInfected) {
            p.infected = true
            p.infectionTime = currentTime
          }
        }
      } else if (p.immune && p.inHealthyPeriod()) {
        p.immune = false
        p.sick = false
        p.infected = false
        p.infectionTime = -1000
      } else if (p.sick && p.inImmunePeriod()) {
        p.sick = false
        p.infected = true
        p.immune = true
        p.deathProbabilityHappened = false
      } else if (p.sick && p.inDiePeriod()) {
        if (!p.deathProbabilityHappened) {
          val willDie = probability(deathRate)
          p.deathProbabilityHappened = true
          if (willDie) {
            p.dead = true
            p.infected = true
            p.sick = false
            p.immune = false
          }
        }
      } else if (p.infected && p.inSickPeriod()) {
        p.infected = true
        p.sick = true
        p.immune = false
      }
    )
    scheduleLifecycle()
  }

  def scheduleLifecycle() {
    afterDelay(1)(lifecycle)
  }
}
