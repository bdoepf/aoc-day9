package de.bdoepf


case class State(activeIndex: Int = 0, v: Vector[Int], field: List[Int] = List(0)) {

  def turn(round: Int): State = {
    if (round % 23 == 0) {
      counterClockWiseTurn(round)
    } else {
      clockWiseTurn(round)
    }
  }

  private def counterClockWiseTurn(round: Int): State = {
    val actualPlayer = round % v.size
    val indexCounter = this.counterClockWiseIndex()
    val headList = this.field.slice(0, indexCounter)
    val removedMarble :: tailList = this.field.slice(indexCounter, this.field.size)
    val newV = v.updated(actualPlayer, v(actualPlayer) + round + removedMarble)
    State(indexCounter, newV, headList ++ tailList)
  }

  private def counterClockWiseIndex(): Int = {
    if (activeIndex - 7 < 0) {
      field.size + activeIndex - 7
    } else {
      activeIndex - 7
    }
  }

  private def clockWiseTurn(round: Int): State = {
    val l = this.clockWiseIndex()
    if (l == 0) {
      val newF: List[Int] = this.field :+ round
      val newIndex = this.field.size
      State(newIndex, this.v, newF)
    } else {
      val headList = this.field.slice(0, l) :+ round
      val tailList = this.field.slice(l, this.field.size)
      val newF: List[Int] = headList ++ tailList
      val newIndex = l
      State(newIndex, this.v, newF)
    }
  }

  private def clockWiseIndex(): Int = {
    (activeIndex + 2) % (field.size)
  }

}


object App {
  def printState(state: State, player: String) = {
    val str = state.field.zipWithIndex.map { case (element, index) =>
      val a = if (index == state.activeIndex) {
        s"(${element})"
      } else {
        element.toString
      }
      f"$a%4s"
    }.mkString("")

    println(s"[$player] $str")
  }


  def main(args: Array[String]): Unit = {
    val numPlayers = 441
    val marbleUntil = 71032

    val initVector = Vector.fill(numPlayers)(0)
    val initState = State(0, initVector)
    // val marbleUntil = 25 // 71032

    val finalState = (1 to marbleUntil).foldLeft(initState)((state: State, round: Int) => {
      state.turn(round)
    })

    println(finalState.v.max)
    assert(finalState.v.max == 393229)
  }

}
