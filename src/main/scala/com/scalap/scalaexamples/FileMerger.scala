package com.scalap.scalaexamples

import java.io.File
import scala.collection.parallel._
import scala.util.matching.Regex
import org.apache.commons.io.FileUtils
import collection.JavaConverters._
import scala.collection.JavaConversions._
import java.nio.file.StandardCopyOption.REPLACE_EXISTING
import java.nio.file.Files.copy
import java.nio.file.Paths.get
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.Paths
import java.nio.file.CopyOption

object FileMerger {

  def main(args: Array[String]) {

    val baseDirPRDX =  "/mapr/mdrnissidhi/MDR/PRD/PRDX/CINCENT/" ///mapr/mdrnissidhi/Incremental/bigPRD"
    val baseDirPRDY =  "/mapr/mdrnissidhi/MDR/PRD/PRDY/CINCENT/"
    val baseDirPRDZ =  "/mapr/mdrnissidhi/MDR/PRD/PRDZ/CINCENT/"

    val customerIdsPRDX = Array("28", "817")
    val customerIdsPRDY = Array("762", "818")
    val customerIdsPRDZ = Array("1224")

    val destDir = "/mapr/mdrnissidhi/Incremental/mergeIncr"
    val pod = args(0)
    if(!("PRDX".equals(pod) || "PRDY".equals(pod) || "PRDZ".equals(pod))) throw new IllegalArgumentException("Invalid pod name")
    val baseFile = new File("/mapr/mdrnissidhi/MDR/PRD/"+pod+"/CINCENT/")

    //.*baseline/.*tsv
    //val files = recursiveFileSearch(baseFile, "xc_.*baseline".r)
    //val files3 = FileUtils.listFiles(baseFile, Array[String]("tsv"), true).asScala
    //files.foreach(println)

    //recursiveFileCopy(baseFile, new File(destDir), "xc_.*baseline.*tsv".r)

    
    val regex = ".*xc_.*baseline.*tsv|.*xc_.*partition.*tsv"
      
    val busList = if(args.length > 1) args(1).split(",") else Array[String]("817")
        
    baseFile.listFiles().par.foreach(busDir => {
      val busId = busDir.getName.substring(busDir.getName.lastIndexOf('/')+1, busDir.getName.length())
      if(busList.contains(busId)){
        println("Copying busienss data: "+busId)
        recursivePartitionFileCopy(busDir, new File(destDir), regex)  
      }      
    })
   /* customerIdsPRDZ.par.foreach(busId => {
      recursiveFileCopy(new File(baseDirPRDZ + busId), new File(destDir), "xc_.*baseline.*tsv".r)
    })*/
  }

  def recursiveFileSearch(f: File, regex: Regex): Array[File] = {

    val files = f.listFiles
    val good = files.filter(file => regex.findFirstIn(file.getPath).isDefined)
    good.foreach(println)
    good ++ files.filter(_.isDirectory()).toStream.flatMap(recursiveFileSearch(_, regex))

  }
  
  
  
  def recursivePartitionFileCopy(src: File, dest: File, regex: String) {

    val files = src.listFiles()

    val regexValidFiles = files.filter(file => file.getPath.matches(regex))
    val partitionFiles = regexValidFiles.filter(file => file.getPath.matches(".*partition.*"))
    if(partitionFiles.length > 0) {
      
      partitionFiles.par.foreach( f => {
        val fileNameParts = f.getAbsolutePath.split("\\/").reverse.take(5)
        val fileName = fileNameParts(0)
        val periodId = fileNameParts(1)
        val tableName = fileNameParts(3)
        val busId = fileNameParts(4)
        val destPath = dest.getAbsolutePath + "/" + tableName + "/" + busId + "_" + periodId +"_"+fileName
  
        val destFile = new File(destPath)
        if(!destFile.exists()) {
          println(s"copying $f to $destPath")
          destFile.mkdirs
          //Files.copy(Paths.get(f.getAbsolutePath), Paths.get(destPath), REPLACE_EXISTING)
        }
      })
      
    } else {
      
      regexValidFiles.par.foreach( f => {
        val fileNameParts = f.getAbsolutePath.split("\\/").reverse.take(4)
        val destPath = dest.getAbsolutePath + "/" + fileNameParts(2) + "/" + fileNameParts(3)+"_"+fileNameParts(0)
  
        val destFile = new File(destPath)
        if(!destFile.exists()) {
          println(s"copying $f to $destPath")
          destFile.mkdirs
          //Files.copy(Paths.get(f.getAbsolutePath), Paths.get(destPath), REPLACE_EXISTING)
        }
      })
      
    }
    
    files.filter(f => {
        val parent = f.getParent
        f.isDirectory() && !(f.getName.matches(".*baseline.*") && new File(f.getParent + "/partition").exists())
      }).foreach(recursivePartitionFileCopy(_, dest, regex))
    

  }

  def recursiveFileCopy(src: File, dest: File, regex: Regex) {

    val files = src.listFiles()

    val good = files.filter(file => regex.findFirstIn(file.getPath).isDefined)
    
    good.par.foreach( f => {
      val fileNameParts = f.getAbsolutePath.split("\\/").reverse.take(4)
      val destPath = dest.getAbsolutePath + "/" + fileNameParts(2) + "/" + fileNameParts(1) + "/" + fileNameParts(3)+"_"+fileNameParts(0)
      println(s"copying $f to $destPath")

      val destFile = new File(destPath)
      if(!destFile.exists()) {
        destFile.mkdirs
        Files.copy(Paths.get(f.getAbsolutePath), Paths.get(destPath), REPLACE_EXISTING)
      }
    })
    files.filter(_.isDirectory()).foreach(recursiveFileCopy(_, dest, regex))

  }

}