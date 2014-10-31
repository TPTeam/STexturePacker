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
    implicit val files = inputDir.listFiles().toList
         
   	val first = 
   	  		ImageIO.read(files.head)
   	
   	val nImages = files.length		
   	  		
   	val root: Node = 
   	  		new Leaf(Rectangle(0,0,first.getWidth,first.getHeight),None)
   	    
   	val result = parseFiles(root,files)
   	
   	(result) match {
      case Some(res) => 
        val better = makeItBetter(res)
        println("ESCIII!!!")
        better
      case _ => throw new Exception("No result found, integer limit reached?")
    }
  }  
  
  def makeItBetter(last_result: Node)(implicit fl: List[File]): Node = {
    val lessW =
      new Leaf(Rectangle(0,0,last_result.rec.w-1,last_result.rec.h),None)
    val lessH =
      new Leaf(Rectangle(0,0,last_result.rec.w,last_result.rec.h-1),None)
    
    val resLessH =
      parseFiles(lessH,fl).map(x => {
       if (x.rec.h+x.rec.w > last_result.rec.h+last_result.rec.w)
         None
       else 
         Some(x)
      }).flatten
    val resLessW =
      parseFiles(lessW,fl).map(x => {
       if (x.rec.h+x.rec.w > last_result.rec.h+last_result.rec.w)
         None
       else 
         Some(x)
      }).flatten
      

    println("Making better! Last_result is "+last_result.rec)
    (resLessH,resLessW) match {
      case (Some(rlh),Some(rlw)) =>
        if (rlh.rec.w>rlw.rec.w)
          makeItBetter(rlh)
        else
          makeItBetter(rlw)
      case (Some(rlh),None) =>
        makeItBetter(rlh)
      case (None,Some(rlw)) =>
        makeItBetter(rlw)
      case _ =>
        println("Nothing can be done better...")
        last_result
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
  
  private def parseFiles(original_root: Node, original_fs: List[File]): Option[Node] = {
    
    
		  def _parseFiles(root: Node, fs: List[File]): (Option[Node], Option[BufferedImage]) = {
				  if (fs.isEmpty) {
				    (Some(root),None)
				  } else {
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
		  
		  def parse(n: Node): List[Rectangle] = {
		    n match {
		      case l: Leaf => parseL(l)
		      case ot: OptionTree => parseOT(ot)
		      case t: Tree => parseT(t)
		    }
		  }
		  
		  def parseL(in: Leaf): List[Rectangle] = {
		    in.getImageName match {
		      case Some(name) => List()
		      case _ => List(in.rec)
		    }
		  }
		  
		  def parseOT(ot: OptionTree) = (
		      parse(ot.tree1.l) :::
		      parse(ot.tree1.r) :::
		      parse(ot.tree2.l) :::
		      parse(ot.tree2.r)
		  )
		  
		  def parseT(in: Tree): List[Rectangle] = {
		      parse(in.l) :::
		      parse(in.r)
		  }
		  
		  
	val result = _parseFiles(original_root, original_fs)
	(result) match {
	  case (Some(res),_) => Some(res)	  
	  case (_,Some(toAdd)) =>
	    val desiredH = toAdd.getHeight()
	    val desiredW = toAdd.getWidth()
	    
	    val rects = parse(original_root)
	    
	    def findClosest(in: List[Rectangle], actual: Option[Rectangle]): Option[Rectangle] = {
	      if (in.isEmpty) actual
	      else {
	        val margin = 
	        		Math.abs(desiredH - in.head.h) +
	        		Math.abs(desiredW - in.head.w)
	        		
	        actual match {
	          case Some(a) =>
	            val actualMargin = 
	              Math.abs(desiredH - a.h) +
	        	  Math.abs(desiredW - a.w)
	        	  
	          	if (actualMargin > margin)
	          		findClosest(in.tail, actual)
	            else
	                findClosest(in.tail, Some(in.head))
	          case _  =>
	            findClosest(in.tail, None)
	        } 
	      }
	    }
	    val closest = findClosest(rects,None)
	    
	    val plusH =
	      if (closest.isDefined)
	    	  closest.get.h - desiredH
	      else
	    	  desiredH
	    val plusW =
	      if (closest.isDefined)
	    	 closest.get.w - desiredW
	      else
	    	 desiredW   

	    println("Actual rectangle "+original_root.rec)
	    	 
	    if ((original_root.rec.w) > (original_root.rec.h*2) ||
	        (original_root.rec.w*2) > (original_root.rec.h)
	        ) {
	      if (plusH < plusW) {
	    	  parseFiles(
			   new Leaf(
			       Rectangle(0,0,
			           (original_root.rec.w+original_root.rec.h)/2,
			           ((original_root.rec.w+original_root.rec.h)/2)+plusH),
			        None)
			,original_fs)
	      } else {
	        parseFiles(
			   new Leaf(
			       Rectangle(0,0,
			           ((original_root.rec.w+original_root.rec.h)/2)+plusW,
			           (original_root.rec.w+original_root.rec.h)/2),
			        None)
			,original_fs)
	      }
	    } else if (plusH < plusW) {
	      parseFiles(
			   new Leaf(
			       Rectangle(0,0,
			           original_root.rec.w,
			           original_root.rec.h+plusH),
			        None)
			,original_fs)
	    } else {
	      parseFiles(
			   new Leaf(
			       Rectangle(0,0,
			           original_root.rec.w+plusW,
			           original_root.rec.h),
			        None)
		  ,original_fs)
	    }	    	
	  case _ => None
	}
  }

}  
