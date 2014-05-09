package com.tecniplast.texturepacker

import java.io._
import java.awt.image._
import javax.imageio._

case class TextureCreator(inputDirName: String, outputDirName: String, outputFileName: String) {
  
  val inputDir = new File(inputDirName)
  val outputDir = new File(outputDirName)
  if (!outputDir.exists()) {
    outputDir.mkdir();
  }
  
  val outputTexture = new File(outputDir,outputFileName+".png")
  val outputJson = new File(outputDir,outputFileName+".json")
  
 def makeTree: Node = {
    val files = inputDir.listFiles().toList
         
   	val first = 
   	  		ImageIO.read(files.head)
   	
   	val nImages = files.length		
   	  		
   	val root: Node = 
   	  		new Leaf(Rectangle(0,0,first.getWidth,first.getHeight),None)
   	    
   	val result = parseFiles(root,files,(first.getWidth,first.getHeight))
    
   	(result) match {
      case Some(res) => res
      case _ => throw new Exception("No result found, integer limit reached?")
    }
  }
  
    
  private def getAverages(list: List[File]): (Int, Int) =
  {
    val i = ImageIO.read(list.head)
    (list.length) match
    {
    	case 1 => (i.getHeight, i.getWidth)
    	case _ => 
    	{
    	  val v = getAverages(list.tail) 
    	  (i.getHeight+v._1,i.getWidth+v._2)
    	}
    }     
  }
  
  
  def printTexture(root: Node) = {
    val printables = listResults(root)
    
    val sprite = 
      new BufferedImage(root.rec.w, root.rec.h.toInt,BufferedImage.TYPE_4BYTE_ABGR)
    
    printables.foreach(img => {
	 val temp = sprite.getSubimage(
			 		img.rec.x.toInt, img.rec.y.toInt, img.rec.w.toInt, img.rec.h.toInt)
	 temp.setData(ImageIO.read(new File(img.image_name)).getData())
	 temp.flush
	 sprite.flush
	 System.gc
    })
  
    ImageIO.write(sprite,"png" , outputTexture);
  }
  
  def composeJson(root: Node): Unit = {
    val printables = listResults(root)
    
    def _composeJson(prints: List[FullLeaf], res: String): String = {
      if (prints.isEmpty) 
        (res + "}," +
        		"\"meta\": {"+
        			"\"image\":\""+outputTexture.getName+"\","+
        			"\"size\": {\"w\":"+root.rec.w+",\"h\":"+root.rec.h+"},"+
        			"\"scale\":\"1\""+
        		"}}")
      else {
        val l = prints.head
        val wh = "\"w\":"+l.rec.w+",\"h\":"+l.rec.h
        val json = 
        			"\""+l.image_name+"\":{"+
        			"\"frame\":{\"x\":"+l.rec.x+",\"y\":"+l.rec.y+","+wh+"},"+
        			"\"rotated\": false, \"trimmed\": false,"+
        			"\"spriteSourceSize\":{\"x\":0,\"y\":0,"+wh+"},"+
        			"\"sourceSize\" : {"+wh+"}"+
        			"}"
        val virg = 
        	if (!prints.tail.isEmpty) ","
        	else ""
        	  
        _composeJson(prints.tail,res + json +virg)
      } 
    }
    
    val initString = "{\"frames\": {"
    val json = _composeJson(printables,initString)
      
      if (outputJson.exists)
    	  outputJson.delete
        	        	
      val pw = new PrintWriter(outputJson)
        	
      pw.println(json)
      pw.flush()
      pw.close()
    }
  
  private def listResults(root: Node): List[FullLeaf] = {
    (root) match {
      case leaf: FullLeaf => List(leaf)
      case tree : Tree => (listResults(tree.l) ::: listResults(tree.r))
      case _ => List() // Only empty leaf goes there
    }
  }
  
  private def parseFiles(original_root: Node, original_fs: List[File], avg: (Int, Int)): Option[Node] = {
    
    
		  def _parseFiles(root: Node, fs: List[File]): (Option[Node], Option[BufferedImage]) = {
				  if (fs.isEmpty) (Some(root),None)
				  else {
					val file = fs.head
					val imageName = file.getAbsolutePath
					val toAdd = ImageIO.read(file)
					val new_root = root.insert(imageName, toAdd.getWidth, toAdd.getHeight)
      
					(new_root) match {
						case Some(nr) => 
							_parseFiles(nr,fs.tail)
						case _ =>
						  (None,Some(toAdd))
					}
				  }
		  }
		  
	val result = _parseFiles(original_root, original_fs)
	(result) match {
	  case (Some(res),_) => Some(res)
	  case (_,Some(toAdd)) =>
	   //if ((original_root.rec.w)>(original_root.rec.h)) {
	  //   if((original_root.rec.w*(original_root.rec.h+toAdd.getHeight))<((original_root.rec.w+toAdd.getWidth)*original_root.rec.h)) { 
	    	parseFiles(
			   new Leaf(
			       Rectangle(0,0,
			           original_root.rec.w+((avg._2*6)/10),//toAdd.getWidth/3,
			           original_root.rec.h+((avg._1*8)/10)),//toAdd.getHeight/3),
			        None)
			,original_fs,avg)
	   //} else {
	/*	    parseFiles(
		    	new Leaf(
				    Rectangle(0,0,
				       original_root.rec.w+toAdd.getWidth,
				       original_root.rec.h),
				    None)
			,original_fs) match {
		      case Some(x) =>Some(x)
		      case _ =>
		        parseFiles(
			   new Leaf(
			       Rectangle(0,0,
			           original_root.rec.w,
			           original_root.rec.h+toAdd.getHeight),
			        None)
			,original_fs)
		    }
			} */
			/*)
			match
			{
			  case (Some(w),Some(h)) => 
			    if((original_root.rec.w*(original_root.rec.h+toAdd.getHeight))>((original_root.rec.w+toAdd.getWidth)*original_root.rec.h)) 
			      Some(w)
			    else
			      Some(h)
			  case (Some(w),None) => Some(w)
			  case (None,Some(h)) => Some(h)
			  case _ => parseFiles(
		    	new Leaf(
				    Rectangle(0,0,
				       original_root.rec.w+toAdd.getWidth,
				       original_root.rec.h+toAdd.getHeight),
				    None),original_fs)
			  
			}*/
	   //}
	  case _ => None
	}
  }

}  
