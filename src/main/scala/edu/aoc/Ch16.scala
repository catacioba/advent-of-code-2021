package edu.aoc

object Ch16 extends AocSolver {

  override def challenge(): String = "ch16"

  case class PacketHeader(version: Long, typeId: Long)

  case class Packet(header: PacketHeader, value: PacketValue) {
    def versionSum(): Long = {
      header.version + (value match {
        case PacketValue.Literal(_) => 0
        case PacketValue.Operator(_, _, sp) => sp.map(_.versionSum()).sum
      })
    }

    private def literal(): Long = value match {
      case PacketValue.Literal(v) => v
    }

    private def doForSubPackets(f: Seq[Long] => Long): Long = value match {
      case PacketValue.Operator(_, _, sp) => f(sp.map(_.compute()))
    }

    def compute(): Long = {
      header.typeId match {
        case 0 => doForSubPackets(_.sum)
        case 1 => doForSubPackets(_.product)
        case 2 => doForSubPackets(_.min)
        case 3 => doForSubPackets(_.max)
        case 4 => literal()
        case 5 => doForSubPackets(l => if l.head > l(1) then 1 else 0)
        case 6 => doForSubPackets(l => if l.head < l(1) then 1 else 0)
        case 7 => doForSubPackets(l => if l.head == l(1) then 1 else 0)
      }
    }
  }

  enum PacketValue:
    case Literal(value: Long)
    case Operator(mode: Long, value: Long, subPackets: Seq[Packet])

  object Packet {
    def parsePacket(s: String): (Packet, String) = {
      val packetHeader = PacketHeader.parseHeader(s.substring(0, 6))

      val (packetValue, remaining): (PacketValue, String) = if packetHeader.typeId == 4 then {
        PacketValue.parseLiteral(s.substring(6))
      } else {
        PacketValue.parseOperator(s.substring(6))
      }

      (Packet(packetHeader, packetValue), remaining)
    }
  }

  object PacketHeader {
    def parseHeader(s: String): PacketHeader = {
      PacketHeader(
        binaryToDecimal(s.substring(0, 3)),
        binaryToDecimal(s.substring(3, 6)))
    }
  }

  object PacketValue {
    def parseLiteral(s: String): (PacketValue.Literal, String) = {
      def parseLiteralAux(s: String): (List[String], String) = {
        s.head match
          case '0' => (s.substring(1, 5) :: Nil, s.substring(5))
          case '1' =>
            val (ss, remaining) = parseLiteralAux(s.substring(5))
            (s.substring(1, 5) :: ss, remaining)
      }

      val (valueParts, remaining) = parseLiteralAux(s)
      val valueString = valueParts.mkString
      (PacketValue.Literal(binaryToDecimal(valueString)), remaining)
    }

    def parseOperator(s: String): (PacketValue.Operator, String) = {
      val mode = s.head.asDigit
      val ss = s.drop(1)

      val (len, r) = mode match {
        case 0 => (binaryToDecimal(ss.substring(0, 15)).toInt, ss.substring(15))
        case 1 => (binaryToDecimal(ss.substring(0, 11)).toInt, ss.substring(11))
      }

      def parseUntilEmpty(s: String): List[Packet] = {
        if s.isBlank then Nil
        else {
          val (p, r) = Packet.parsePacket(s)
          p :: parseUntilEmpty(r)
        }
      }

      def parseByCount(s: String, c: Long): (List[Packet], String) = {
        if c <= 0 then (Nil, s) else {
          val (p, r) = Packet.parsePacket(s)
          val (pp, rr) = parseByCount(r, c - 1)
          (p :: pp, rr)
        }
      }

      val (subPackets, remaining) = mode match {
        case 0 => (parseUntilEmpty(r.substring(0, len)), r.substring(len))
        case 1 => parseByCount(r, len)
      }

      (PacketValue.Operator(mode, len, subPackets), remaining)
    }
  }

  override def solvePart1(input: Seq[String]): Unit = {
    val versionSum = input.map(hexToBinary)
                          .map(s => Packet.parsePacket(s)._1)
                          .map(_.versionSum())
    println(versionSum)
  }

  override def solvePart2(input: Seq[String]): Unit = {
    val computedValue = input.map(hexToBinary)
                             .map(s => Packet.parsePacket(s)._1)
                             .map(_.compute())
    println(computedValue)
  }


  private val HEX_TO_BINARY = Map(
    '0' -> "0000",
    '1' -> "0001",
    '2' -> "0010",
    '3' -> "0011",
    '4' -> "0100",
    '5' -> "0101",
    '6' -> "0110",
    '7' -> "0111",
    '8' -> "1000",
    '9' -> "1001",
    'A' -> "1010",
    'B' -> "1011",
    'C' -> "1100",
    'D' -> "1101",
    'E' -> "1110",
    'F' -> "1111")

  private def hexToBinary(hex: String): String = {
    hex.flatMap(HEX_TO_BINARY.apply)
  }

  private def binaryToDecimal(binary: String): Long = {
    java.lang.Long.parseLong(binary, 2)
  }
}
