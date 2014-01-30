package utils

import org.joda.time.DateTime
import java.util._
import org.ocpsoft.prettytime.PrettyTime
import org.joda.time.DateTimeZone

object Time {

  val zone = DateTimeZone.forID("Europe/Amsterdam")
  val tz = TimeZone.getTimeZone("Europe/Amsterdam")

  def ordinal(date: DateTime) = {
    val cal = Calendar.getInstance()
    cal.setTime(date.toDateTime(zone).toDate)
    cal.setTimeZone(tz)
    val num = cal.get(Calendar.DAY_OF_MONTH)
    val suffix = Array("th", "st", "nd", "rd", "th", "th", "th", "th", "th", "th")
    val m = num % 100
    val index = if(m > 10 && m < 20){ 0 } else { (m % 10) }
    num.toString + suffix(index)
  }

  def timeAgo(date: DateTime) = {
    val p = new PrettyTime()
    p.format(date.toDate)
  }

  def md5(s: String) = {
      import java.security.MessageDigest
      MessageDigest.getInstance("MD5").digest(s.getBytes).map("%02X".format(_)).mkString.toLowerCase
  }
  

}