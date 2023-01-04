package com.github.gekomad.chessgengenerator.board.perft

object Perft {
  import com.github.gekomad.chessgengenerator.board.Board._
  import com.github.gekomad.chessgengenerator.board.core.ChessBoard._
  import com.github.gekomad.chessgengenerator.board.core.{ChessBoard, GenMoves}

  def printAndSum(l: Option[Seq[(String, BitmapPosition)]], s: Long = 0): Long = {
    println()
    l.map { x =>
      val o = (x zip (1 to 10000)).map { a =>
        println(s"${a._2})\t${a._1._1}\t${a._1._2}")
        a._1._2
      }
      val tot = o.sum
      print(s"Tot $tot nodes")
      if (s != 0)
        print(s" in $s seconds (${(tot / 1000) / s}k nodes per seconds)")
      println()

      tot
    }
  }.getOrElse(-1)

  private def calculate(idListPre: Int, g: GenMoves, side: Int, useHash: Boolean, smp: Boolean, depth: Int): Long = {
    assert(side == 0 || side == 1)
    if (depth == 0) 1
    else {

      val idList = idListPre + 1
      g.generate(idList, side)
        .map { move =>
          val keyold = g.zobristKey
          g.makemove(move, rep = false, checkInCheck = false)
          val nPerft = calculate(idList, g, side ^ 1, useHash, smp, depth - 1)
          g.takeback(move, keyold, rep = false)
          nPerft
        }
        .sum
    }
  }

  def perft(fen: String, depth: Int, print: Boolean = false): Option[Seq[(String, BitmapPosition)]] =
    GenMoves(fen, true)
      .map { g =>
        g.display()

        val side = g.getSide
        val idList = 0
        val fhash = false
        val smp = false

        val res = g.generate(idList, side).map { move =>
          val keyold = g.zobristKey
          g.makemove(move, rep = false, checkInCheck = false)

          val nPerft = calculate(idList, g, side ^ 1, fhash, smp, depth = depth - 1)

          g.takeback(move, keyold, false)

          val h =
            if (ChessBoard.decodeBoardinv(move.type1, move.to, side).length() > 2) {
              //castle
              ChessBoard.decodeBoardinv(move.type1, move.to, side)

            } else {
              ChessBoard.decodeBoardinv(move.type1, move.from, side) + (if (move.capturedPiece != SQUARE_FREE) '*' else '-') +
                ChessBoard.decodeBoardinv(move.type1, move.to, side)
            }
          if (print) println(h + " " + nPerft)
          (h, nPerft)

        }

        Some(res)
      }.flatten

}
