package com.tecniplast.texture

import java.io._
import java.awt.image._
import javax.imageio._

object Test extends App {
  println("Start")
  
  val tc = new TextureCreator("/home/andrea/ftp/Upload/animation","greit")
  val node = tc.makeTree
  tc.composeJson(node)
  tc.printTexture(node)

  println("End")
}