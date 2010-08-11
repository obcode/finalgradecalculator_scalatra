package org.obraun.finalgradecalculator

import org.scalatra._

class FGCFilter extends ScalatraFilter {

  get("/calculate/:grade/:mark1/:mark2/:mark3") {

    val grade = params("grade")
    val mark1 = params("mark1") replace (',','.') toDouble
    val mark2 = params("mark2") replace (',','.') toDouble
    val mark3 = params("mark3") replace (',','.') toDouble

    def isValid(mark: Double) =
      (1.0 <= mark) && (mark <= 5.0)

    if (!isValid(mark1) ||
        !isValid(mark2) ||
        !isValid(mark3)) redirect("/error")

    def rating(m: Double): String = {
      val adjusted = (m*10).toInt/10.0
      if (adjusted <= 1.2) return "mit Auszeichnung bestanden"
      if (adjusted <= 1.5) return "sehr gut bestanden"
      if (adjusted <= 2.5) return "gut bestanden"
      if (adjusted <= 3.5) return "befriedigend bestanden"
      if (adjusted <= 4.0) return "ausreichend bestanden"
      return "nicht bestanden"
    }

    def calcMark(p1: Double, p2: Double, p3:Double) = {
      require(p1+p2+p3 == 100)
      val mark = (
        (mark1.toDouble) * p1 +
        (mark2.toDouble) * p2 +
        (mark3.toDouble) * p3
      ) / 100
      mark.toString.replace('.',',')+", "+rating(mark)
    }

    grade.map{_.toLower} match {
      case "ba" =>
        <h1>Bachelor of Science:
          {calcMark(6,22,72)}
        </h1>
      case "dipl" =>
        <h1>Dipl.-Inform.:
          {calcMark(32,8,60)}
        </h1>
      case _ => redirect("/error")
    }

  }

  def calculateForm(
    gradename: String, grade: String,
    mark1: String, mark2: String, mark3: String
  ) = {
    <body>
      <h1>{gradename}</h1>
      <form action='/post' method='POST'>
        <input name='grade' value={grade} type='hidden'/>
        <table>
          <tr>
          <tr>
            <td>{mark1}:</td>
            <td><input name='mark1' type='text'/></td>
          </tr>
          <tr>
            <td>{mark2}:</td>
            <td><input name='mark2' type='text'/></td>
          </tr>
          <tr>
            <td>{mark3}:</td>
            <td><input name='mark3' type='text'/></td>
          </tr>
            <td></td>
            <td><input type='submit'/></td>
          </tr>
        </table>
      </form>
    </body>
  }

  get("/calculate/ba") {
    calculateForm(
      "Bachelor of Science",
      "ba",
      "Note der Bachelorarbeit",
      "Fachprüfungsgesamtnote",
      "Mittlere Note aller Module"
    )
  }

  get("/calculate/dipl") {
    calculateForm(
      "Diplom-Informatiker(in)",
      "dipl",
      "Note der Diplomarbeit",
      "Note des Kolloquiums",
      "Fachprüfungsgesamtnote"
    )
  }

  post("/post") {
    redirect("/calculate/"+
      params("grade")+"/"+
      params("mark1")+"/"+
      params("mark2")+"/"+
      params("mark3")
    )
  }

  get("/") {
    <body>
      <h1>Final-Grade-Calculator</h1>
      <form action='/postgrade' method='POST'>
        <input type='radio' name='grade' value='ba' />Bachelor of Science
        <input type='radio' name='grade' value='dipl' />Diplom-Informatiker
        <input type='submit'/>
      </form>
    </body>
  }

  post("/postgrade") {
      redirect("/calculate/"+params("grade"))
  }

  notFound {
    "Falsche Eingabe"
  }
}
// vim: set ts=2 sw=4 et:
