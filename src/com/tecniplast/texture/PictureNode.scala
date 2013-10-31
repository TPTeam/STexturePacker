package com.tecniplast.texture

// Algorithm from http://www.blackpawn.com/texts/lightmaps/
// made functional
abstract class Node(rect: Rectangle) {
  
  def rec = rect
  
  def insert(imageName: String, w: Int, h: Int): Option[Node]
  
}

case class Tree(rect: Rectangle,
				l: Node,
				r: Node) extends Node(rect) {

  def insert(imageName: String, w: Int, h: Int): Option[Node] =
    (l.insert(imageName,w,h)) match {
    	case Some(nl) =>
    		Some(Tree(rect,nl,r))
        case _ =>
          (r.insert(imageName,w,h)) match {
          	case Some(nr) =>
          	  Some(Tree(rect,l,nr))
          	case _ => None
      }
    }  
  
}

class Leaf(rect: Rectangle,
				image_name: Option[String]
				) extends Node(rect) {
  def insert(imageName: String, w: Int, h: Int): Option[Node] =
	if (image_name.isDefined) None		// busy
    else if (rect.w == w && rect.h == h) Some(FullLeaf(rect,imageName))	// fullfilled leaf
    else if (rect.w>=w && rect.h>=h) {	//becoming tree
      if ((rect.w - w) > (rect.h - h)) //split orizz
    	  Some(
    	      Tree(rect,
    	    	   Tree(rect,
    	    	       EmptyLeaf(Rectangle(rect.x+w,rect.y,rect.w-w,h)),
    	    	       EmptyLeaf(Rectangle(rect.x,rect.y+h,rect.w,rect.h-h))
    	    	       ),
    	    	   FullLeaf(Rectangle(rect.x,rect.y,w,h), imageName)
    			  )
    		)
      else							//split vert
    	  Some(
    	      Tree(rect,
    	    	   Tree(rect,
    	    	       EmptyLeaf(Rectangle(rect.x,rect.y+h,w,rect.h-h)),
    	    	       EmptyLeaf(Rectangle(rect.x+w,rect.y,rect.w-w,rect.h))
    	    	       ),
    	    	   FullLeaf(Rectangle(rect.x,rect.y,w,h), imageName)
    			  )
    		)
    } else None 
}

case class FullLeaf(rect: Rectangle,
					image_name: String
				) extends Leaf(rect,Some(image_name))
case class EmptyLeaf(rect: Rectangle
				) extends Leaf(rect,None)

case class Rectangle(x: Int,y: Int,w: Int,h: Int) {
  override def toString() = {
    "Rect["+x+","+y+","+w+","+h+"]"
  }
}