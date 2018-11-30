import java.io.File

import scala.io.Source

object Main extends App {
  println("Table generator for paper. Input log files, output tex rows.")
  for (f <- 1 to 10) {
    val qsd = List(4, 7, 10) map { side =>
      def parse(arq: String) = {
        //res:    10178   3658.37 8095.661        6464    2744    889     81      10178   90      RationalQuadratic(alpha=1,_length_scale=1)      100.0   99.697  [(1.0, 1.0), (0.12, 1.0), (1.0, 0.12), (0.61, 1.0), (1.0, 0.61), (0.37, 1.0), (1.0, 0.37), (0.81, 1.0), (1.0, 0.81), (0.0, 0.92), (0.92, 0.0), (0.12, 0.12), (0.12, 0.38), (0.63, 0.37), (0.62, 0.63), (0.13, 0.63), (0.62, 0.13), (0.37, 0.12), (0.38, 0.63), (0.37, 0.38), (0.85, 0.63), (0.24, 0.9), (0.9, 0.24), (0.63, 0.85), (0.86, 0.41), (0.41, 0.86), (0.86, 0.86), (0.82, 0.12), (0.12, 0.82), (1.0, 0.49), (0.49, 1.0), (0.63, 0.0), (0.0, 0.62), (0.0, 0.13), (0.12, 0.0), (0.38, 0.0), (0.0, 0.37), (0.91, 0.73), (0.73, 0.91), (0.5, 0.37), (0.13, 0.25), (0.62, 0.5), (0.38, 0.25), (1.0, 0.25), (0.25, 1.0), (0.12, 0.51), (0.49, 0.12), (0.26, 0.62), (0.24, 0.13), (0.37, 0.51), (0.24, 0.37), (0.0, 1.0), (1.0, 0.0), (0.5, 0.62), (0.75, 0.37), (0.37, 0.75), (0.63, 0.25), (0.73, 0.62), (0.52, 0.89), (0.89, 0.52), (0.63, 0.74), (0.94, 0.92), (0.72, 0.12), (0.71, 1.0), (1.0, 0.71), (0.11, 0.72), (0.9, 1.0), (0.31, 0.84), (0.84, 0.31), (0.11, 0.91), (0.91, 0.11), (0.0, 0.83), (0.83, 0.0), (0.77, 0.83), (0.33, 0.93), (0.93, 0.33), (0.43, 0.44), (0.83, 0.74), (0.56, 0.06), (0.56, 0.43), (0.18, 0.44), (1.0, 0.9), (0.31, 0.06), (0.31, 0.19), (0.69, 0.44), (0.31, 0.44), (0.31, 0.31), (0.07, 0.19), (0.56, 0.19)]   [0, 35, 12, 49, 84, 18, 83, 36, 47, 79, 32, 73, 11, 53, 3, 71, 28, 63, 17, 89, 57, 14, 85, 55, 69, 23, 44, 7, 76, 25, 60, 30, 5, 65, 38, 21, 58, 61, 24, 39, 74, 78, 27, 9, 82, 62, 1, 67, 8, 64, 4, 31, 59, 26, 56, 68, 75, 6, 45, 22, 70, 2, 52, 10, 72, 29, 66, 33, 46, 16, 48, 19, 54, 15, 42, 80, 40, 77, 50, 86, 20, 43, 87, 51, 81, 13, 37, 41, 88, 34]
        val file = s"../ocean/f$f-0-${side}x$side-100-$arq"
        try {
          val content = Source.fromFile(file).getLines().toList
          val lines = content.map { l => l.split('\t').toList }

          val (vars, errors) = content.takeRight(10).map(x => x.split('\t').toList.tail).map {
            case List(a, b) if (a + b).filter(_ != '.') forall Character.isDigit => a.toDouble -> b.toDouble
            case _ =>
              def extract(pos: Int) = {
                lines.dropWhile(x => x(1).toInt < 3600000).head(pos).toDouble
              }

              extract(2) -> extract(3)
          }.unzip
          val (endv, ende) = (vars.sum / vars.length, errors.sum / errors.length)

          val iniv = lines.head(2).toDouble.round
          val oriv = lines.tail.head(2).toDouble.round

          val inie = lines.head(3).toDouble.round
          val orie = lines.tail.head(3).toDouble.round

          (iniv, oriv, endv.round, inie, orie, ende.round)
        } catch {
          case _: Throwable => (0l, 0l, 0l, 0l, 0l, 0l)
        }
      }

      val (iniv, oriv, endv1c, inie, orie, ende1c) = parse("1c-1.off.log")
      val (_, _, endvsw, _, _, endesw) = parse("sw-1.off.log")
      val (_, _, endvon, _, _, endeon) = parse("1c-999999.on.log")

      val vs = List(iniv, oriv, endv1c, endvsw, endvon)
      val Maxv = vs.min
      val vs2 = vs.map {
        case Maxv => s"\\textit{$Maxv}"
        case x => x.toString
      }

      val es = List(inie, orie, ende1c, endesw, endeon)
      val Maxe = es.min
      val es2 = es.map {
        case Maxe => s"\\textbf{$Maxe}"
        case x => x.toString
      }

      vs2.mkString(" & ") -> es2.mkString(" & ")
    }
    val (qsdv, qsde) = qsd.unzip
    println("\\multirow{2}{*}{" + f + "}  & \\textbf{$\\sigma$} & " + qsdv.mkString(" & ") + " \\\\")
    println(" & \\textbf{$\\Delta$} & " + qsde.mkString(" & ") + " \\\\")
  }
}

/*
\multirow{2}{*}{1}  & \textbf{$\sigma$} & 7838 & 2097 & 1849 & 1438 &  & 0000 & 0000 & 0000 & 0000 & 0000 & 0000 & 0000 \\
  & \textbf{$\Delta$}  & 0000 & 0000 & 0000 & 0000 & 0000 & 0 & 0 & 0 & 0 & 0 & 0 & 0 \\
*/
