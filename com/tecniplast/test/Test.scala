package com.tecniplast.test

import java.io._
import java.awt.image._
import javax.imageio._
import com.tecniplast.texturepacker.TextureCreator

object Test extends App {
  println("Start")
  
  //val tc = new TextureCreator("out","./","result")
  val tc = new TextureCreator("/home/andrea/RENDER/PROVA","/home/andrea/RENDER/PROVA/","PROVA")
  
  
  val node = tc.makeTree
  tc.composeJson(node)
  tc.printTexture(node)

  println("End")
}