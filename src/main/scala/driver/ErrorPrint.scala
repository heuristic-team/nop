package nop
package driver

import scala.io.AnsiColor.*
import frontend.lexer.Span

private val UNDERLINE_COLOR = GREEN

def printDiagnostic(filename: String, contents: String, diagnostic: Diagnostic) = {
  println(s"$BOLD$filename$RESET")
  printMsg(
    filename,
    contents,
    diagnostic.span,
    s"${BOLD + RED}error${RESET + BOLD}: ${diagnostic.msg}$RESET",
  )

  for note <- diagnostic.notes do
    printMsg(
      filename,
      contents,
      note.span,
      s"${BOLD + CYAN}note${RESET + BOLD}${note.value}$RESET",
    )
}

private def printMsg(filename: String, contents: String, span: Span, msg: String): Unit = {
  val lineNumber -> columnNumber = {
    val lineBoundaries = getLineBoundaries(contents).zipWithIndex

    val ((b, _), number) = lineBoundaries
      .find((boundaries, _) => boundaries._1 <= span.end && boundaries._2 >= span.start)
      .get
    (number + 1, span.start - b + 1)
  }

  printWithLocation(lineNumber, columnNumber, msg)
  printOverlappingLines(contents, span)
}

private def printWithLocation(lineNumber: Int, columnNumber: Int, msg: String): Unit =
  println(s"$lineNumber:$columnNumber: $msg")

private def printOverlappingLines(contents: String, span: Span): Unit = {
  def flatten(ctx: (((Int, Int), Int), String)): ((Int, Int), Int, String) =
    val ((boundaries, i), line) = ctx
    (boundaries, i, line)

  def isEarlyLine(ctx: ((Int, Int), Int, String)): Boolean =
    val ((_, e), _, _) = ctx
    e < span.start

  def isOverlappingLine(ctx: ((Int, Int), Int, String)): Boolean =
    val ((b, e), _, _) = ctx
    b <= span.end && e >= span.start

  val linesToShow = getLineBoundaries(contents).zipWithIndex
    .zip(contents.linesIterator)
    .map(flatten)
    .dropWhile(isEarlyLine)
    .takeWhile(isOverlappingLine)
    .map((boundaries, i, line) => (i + 1, boundaries, line))

  // loop below is used to make pretty underlines
  // examples:
  //   37 | a := foobar
  //      |      ^~~~~^
  //
  //   42 | a := foo(1,
  //      |      ^~~~~~
  //   43 |          2,
  //      |          ~~
  //   44 |          3)
  //      |          ~^

  var i: Int = 0
  for (number, (start_offset, end_offset), line) <- linesToShow do {
    printPrefix(number)
    println(line)

    printPrefix

    i = start_offset;
    while i < span.start do
      print(" ")
      i += 1

    var startedUnderlining = false;
    if i == span.start then
      print(s"$UNDERLINE_COLOR^")
      startedUnderlining = true
      i += 1

    var lineChars = line.iterator.drop(i - start_offset)
    while i < end_offset && i < span.end do {
      // `getOrElse` is needed because underline may be under a `\n`,
      // which is removed by `.linesIterator`
      if lineChars.nextOption
          .map(_.isWhitespace)
          .getOrElse(false) && !startedUnderlining
      then print(" ")
      else
        print(s"$UNDERLINE_COLOR~")
        startedUnderlining = true
      i += 1
    }

    // `span.start != span.end` to verify that we don't print "^^" under
    // a single character token
    if i == span.end && span.start != span.end then print(s"$UNDERLINE_COLOR^")
    println(RESET)
  }
}

private def getLineBoundaries(source: String): Iterator[(Int, Int)] =
  source.iterator.zipWithIndex
    .filter((c, i) => c == '\n')
    .map((c, i) => i)
    .scanLeft((0, -1)) { (prevRes, l) => (prevRes._2 + 1, l) }
    .drop(1)

private def printPrefix: Unit            = print(" " * 6 + " | ")
private def printPrefix(line: Int): Unit = print(f"$line%-6s | ")
