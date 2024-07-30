import scala.io.StdIn._

object StudentRecordManager {

  // Function to calculate grade based on percentage
  def calculateGrade(percentage: Double): Char = {
    percentage match {
      case p if p >= 90 => 'A'
      case p if p >= 75 => 'B'
      case p if p >= 50 => 'C'
      case _ => 'D'
    }
  }

  // Function to get student information
  def getStudentInfo: (String, Int, Int, Double, Char) = {
    // Read and validate input
    var name: String = ""
    var marks: Int = 0
    var totalMarks: Int = 0

    // Get valid input
    def getValidInput: Unit = {
      println("Enter student's name: ")
      name = readLine().trim
      println("Enter marks obtained: ")
      marks = readInt()
      println("Enter total possible marks: ")
      totalMarks = readInt()
    }

    // Input validation
    def validateInput(name: String, marks: Int, totalMarks: Int): (Boolean, Option[String]) = {
      if (name.isEmpty) {
        (false, Some("Name cannot be empty."))
      } else if (marks < 0 || totalMarks <= 0 || marks > totalMarks) {
        (false, Some("Marks must be non-negative integers and should not exceed total possible marks."))
      } else {
        (true, None)
      }
    }

    // Retry mechanism for valid input
    def getStudentInfoWithRetry: (String, Int, Int, Double, Char) = {
      var valid = false
      var result: (String, Int, Int, Double, Char) = ("", 0, 0, 0.0, 'D')

      while (!valid) {
        getValidInput
        val (isValid, errorMessage) = validateInput(name, marks, totalMarks)
        if (isValid) {
          val percentage = (marks.toDouble / totalMarks) * 100
          val grade = calculateGrade(percentage)
          result = (name, marks, totalMarks, percentage, grade)
          valid = true
        } else {
          errorMessage.foreach(println)
        }
      }

      result
    }

    getStudentInfoWithRetry
  }

  // Function to print student record
  def printStudentRecord(record: (String, Int, Int, Double, Char)): Unit = {
    val (name, marks, totalMarks, percentage, grade) = record
    println(s"Student Name: $name")
    println(s"Marks Obtained: $marks")
    println(s"Total Possible Marks: $totalMarks")
    println(f"Percentage: $percentage%.2f%%")
    println(s"Grade: $grade")
  }

  def main(args: Array[String]): Unit = {
    // Get student information and print record
    val studentRecord = getStudentInfo
    printStudentRecord(studentRecord)
  }
}
