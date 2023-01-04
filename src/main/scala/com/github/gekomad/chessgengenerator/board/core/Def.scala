package com.github.gekomad.chessgengenerator.board.core
import scala.annotation.tailrec

object Def {

  private val lsb64Table = Array(63, 30, 3, 32, 59, 14, 11, 33, 60, 24, 50, 9, 55, 19, 21, 34, 61, 29, 2, 53, 51, 23, 41, 18, 56, 28, 1, 43, 46, 27, 0, 35, 62, 31, 58, 4, 5, 49, 54, 6, 15, 52, 12, 40,
    7, 42, 45, 16, 25, 57, 48, 13, 10, 39, 8, 44, 20, 47, 38, 22, 17, 37, 36, 26)

  /**
    *
    * @param b
    * @author Matt Taylor
    * @return
    */
  @inline final def BITScanForward(b: Long): Int = {
    val bb          = b ^ (b - 1)
    val folded: Int = (bb ^ (bb >>> 32)).toInt
    lsb64Table(folded * 0x78291ACF >>> 26)

  }

  private val index64 = Array(0, 47, 1, 56, 48, 27, 2, 60, 57, 49, 41, 37, 28, 16, 3, 61, 54, 58, 35, 52, 50, 42, 21, 44, 38, 32, 29, 23, 17, 11, 4, 62, 46, 55, 26, 59, 40, 36, 15, 53, 34, 51, 20, 43,
    31, 22, 10, 45, 25, 39, 14, 33, 19, 30, 9, 24, 13, 18, 8, 12, 7, 6, 5, 63)

  /**
    *
    * @param b
    * @authors Kim Walisch, Mark Dickinson
    * @return
    */
  @inline final def BITScanReverse(b: Long): Int = {
    // @authors Kim Walisch, Mark Dickinson
    var bb = b
    bb |= bb >>> 1
    bb |= bb >>> 2
    bb |= bb >>> 4
    bb |= bb >>> 8
    bb |= bb >>> 16
    bb |= bb >>> 32
    index64(((bb * 0x03f79d71b4cb0a89L) >>> 58).toInt)
  }

  @inline final def resetLSB(bits: Long): Long = bits & (bits - 1)

  @tailrec
  @inline final def bitCount(bits: Long, count: Int = 0): Int =
    if (bits == 0) count
    else
      bitCount(bits & (bits - 1), count + 1)

}
