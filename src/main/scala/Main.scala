import java.io.PrintWriter

import scalafx.Includes._

import scala.io.Source
import scalafx.application._
import scalafx.event.ActionEvent
import scalafx.scene._
import scalafx.scene.control._


object Main extends App {
  val out = new PrintWriter("result3.txt")
  //var key = "SENIOR SEMINAR"
  var urlString1 = "https://sunspot.sdsu.edu/schedule/sectiondetails?scheduleNumber="
  var urlString2 = "&period=20192&admin_unit=R"
  var html = Source.fromURL("https://www.google.com/")
  var xmlString = ""
  var t0, t1 = 0.asInstanceOf[Long]
  println("Scanning database\nPlease wait...")
  out.println("Schedule #" + "\t" + "Course Title")
  val temp = "Course Title"
  val gap = "Course Title</td>\n                    <td class=\"sectionDetailContent\">"
  def KMPSearch(pat: String, txt: String): Boolean = {
    val M = pat.length
    val N = txt.length
    var check = false
    // create lps[] that will hold the longest
    // prefix suffix values for pattern
    val lps = new Array[Int](M)
    var j = 0 // index for pat[]
    // Preprocess the pattern (calculate lps[]
    // array)
    computeLPSArray(pat, M, lps)
    var i = 0 // index for txt[]
    while ( {
      i < N
    }) {
      if (pat.charAt(j) == txt.charAt(i)) {
        j += 1
        i += 1
      }
      if (j == M) {
//        println("*************************************************Found pattern " + "at index " + (i - j))
        out.println(txt.substring(i - j + gap.length + 1, i - j + gap.length + 200).split("<").apply(0))
        j = lps(j - 1)
        check = true
      }
      else { // mismatch after j matches
        if (i < N && pat.charAt(j) != txt.charAt(i)) { // Do not match lps[0..lps[j-1]] characters,
          // they will match anyway
          if (j != 0) j = lps(j - 1)
          else i = i + 1
        }
      }
    }
    check
  }

  def computeLPSArray(pat: String, M: Int, lps: Array[Int]): Unit = { // length of the previous longest prefix suffix
    var len = 0
    var i = 1
    lps(0) = 0 // lps[0] is always 0

    // the loop calculates lps[i] for i = 1 to M-1
    while ( {
      i < M
    }) if (pat.charAt(i) == pat.charAt(len)) {
      len += 1
      lps(i) = len
      i += 1
    }
    else { // (pat[i] != pat[len])
      // This is tricky. Consider the example.
      // AAACAAAA and i = 7. The idea is similar
      // to search step.
      if (len != 0) {
        len = lps(len - 1)
        // Also, note that we do not increment
        // i here
      }
      else { // if (len == 0)
        lps(i) = len
        i += 1
      }
    }
  }

  for (id <- 32001 to 36000) {
    //    println("Scanning #" + id)
    html = Source.fromURL(urlString1 + id + urlString2, "UTF-8")
    xmlString = html.mkString
    //println(xmlString)
    out.print(id + "\t\t")
    if (!KMPSearch(temp, xmlString)) out.println("N/A")

    Thread.sleep(100)
  }
  t1 = System.nanoTime()
  println("Elapsed time: " + ((t1 - t0)/1000000000) + " seconds")

  out.close()


}