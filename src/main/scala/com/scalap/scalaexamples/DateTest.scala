package com.scalap.scalaexamples

import java.time.LocalDateTime
import java.time.LocalDate
import java.util.Date
import java.time.ZonedDateTime
import java.time.ZoneId
import java.time.ZoneOffset
import java.time.format.DateTimeFormatter
import java.time.Instant
import scala.util.Try
import java.text.SimpleDateFormat
import java.time.Duration

/**
 * Java8 Date objects
 * https://www.javabrahman.com/java-8/working-with-time-zones-in-java-8-zoneddatetime-zoneid-tutorial-with-examples/
 */
object DateTest {
  
   def main(args: Array[String]) {
    
    val systemDefaultZoneId = ZoneId.systemDefault()
     
    val curTime = System.currentTimeMillis()
    println("curTime: "+ curTime)
    println("cur date based on time: "+ new Date(curTime))
    println("cur date: "+ new Date)
    println("cur time from date: "+ new Date().getTime)
    
    val localDateTime = LocalDateTime.now()
    println("local date time: "+ localDateTime)
    println("local date time epoch: "+ localDateTime.toEpochSecond(ZoneOffset.UTC))
    
    println("local date time: "+  LocalDateTime.now(ZoneOffset.UTC))
    //println("zoned date time from local: "+  LocalDateTime.from(ZoneOffset.UTC))
    
    println("zoned date time: "+ ZonedDateTime.now())
    println("zoned date time: "+ ZonedDateTime.now(ZoneOffset.UTC))
    //val localDateTime = LocalDateTime.now(Zone)
    val dtformatter = DateTimeFormatter.ofPattern("yyyy-MM-dd'T'hh:mm:ss'+00:00'")
    println(ZonedDateTime.now().format(DateTimeFormatter.ofPattern("yyyy-MM-dd'T'hh:mm:ssXXX")))
    println(ZonedDateTime.now(ZoneOffset.UTC).format(DateTimeFormatter.ofPattern("yyyy-MM-dd'T'hh:mm:ss'+00:00'")))
    println(ZonedDateTime.now.withZoneSameInstant(ZoneId.of("UTC")).format(DateTimeFormatter.ofPattern("yyyy-MM-dd'T'hh:mm:ss'+00:00'")))
    //ZoneId.of("UTC") == ZoneOffset.UTC
    //ISO_OFFSET_DATE_TIME
   //ofPattern("yyyy-MM-dd'T'hh:mm:ssXXX") 
    
    //LocalDateTime to Zoned
    val ldt = LocalDateTime.now()
    System.out.println("ldt: "+ldt)
    val ldtZoned = ldt.atZone(ZoneId.systemDefault())
    println("ldtZoned: "+ldtZoned)
    val utcZoned = ldtZoned.withZoneSameInstant(ZoneId.of("UTC"))
    System.out.println("utcZoned: "+utcZoned)
    
    //Epoch timestamp to zoneddatetime
    val instant = Instant.ofEpochSecond(curTime)
    ZonedDateTime.ofInstant(instant, ZoneOffset.UTC).format(DateTimeFormatter.ofPattern("yyyy-MM-dd'T'hh:mm:ss'+00:00'"))
    
    
    val dtFormat = DateTimeFormatter.ISO_OFFSET_DATE_TIME//ofPattern("yyyy-mm-dd'T'hh:mm:ssXXX")//.withZone(ZoneId.of("UTC"))
    val dtFormat2 = DateTimeFormatter.ofPattern("yyyy-MM-dd'T'hh:mm:ss'+00:00'")
    println(ZonedDateTime.parse("2018-04-02T01:00:00+00:00", dtFormat).format(dtFormat2))
    
    val dtFormat4 = DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm:ssZ")
    val modifiedDate = "2018-04-24T20:38:55+0000" //snapShot.modifiedDate
    //val now = ZonedDateTime.parse(modifiedDate, dtFormat4)//.withZoneSameInstant(ZoneOffset.UTC)
    //val localDate = LocalDateTime.parse(modifiedDate, dtFormat4)  
    val zonedDate3 = ZonedDateTime.parse(modifiedDate, dtFormat4)
    println("zonedDate3:" + zonedDate3)
    println("zonedDate3 local:" + zonedDate3.withZoneSameInstant(systemDefaultZoneId))
    println("zonedDate3 local:" + zonedDate3.toLocalDateTime())
    
    /**
     * Java8 requires z or : in zone formate. 00:00 is valie but 0000 is not
     */
    /*val instant2 = Instant.parse(modifiedDate)
    val dateTimeOnly = LocalDateTime.ofInstant(instant2, ZoneId.of(ZoneOffset.UTC.getId()))
    println("dateTimeOnly: " +dateTimeOnly)
    val zonedDateTime = instant2.atZone(ZoneId.of("PST"));
    println("zonedDateTime: " +zonedDateTime)*/
    
    val df = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss'+0000'")
    val date = df.parse(modifiedDate)
    println("date: "+ date)
    
    //Wrong
    val dateInstant = date.toInstant()
    val zonedDateTimeFrmInstant = instant.atZone(systemDefaultZoneId)
    println("zonedDateTimeFrmInstant :"+zonedDateTimeFrmInstant)
    
    //Wrong
    val zonedDateFromDate = ZonedDateTime.ofInstant(dateInstant, systemDefaultZoneId)
    println("zonedDateFromDate :"+zonedDateFromDate)
    println("zonedDateFromDate in PST :"+zonedDateFromDate.withZoneSameInstant(systemDefaultZoneId))
    
    
    val dtformatter2 =  DateTimeFormatter.ofPattern("EEE MMM dd HH:mm:ss z yyyy")
    val maprDt = ZonedDateTime.parse("Tue Aug 21 12:15:03 PDT 2018", dtformatter2)
    val now = ZonedDateTime.now()
    println("maprDt: "+maprDt)
    val dateDiff = Duration.between(maprDt, now).toHours()
    println("now - maprDt: " +dateDiff )
    
  }
  
}