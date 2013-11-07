package com.tecniplast.test

import java.io._
import java.awt.image._
import javax.imageio._
import com.tecniplast.texturepacker.TextureCreator

object Test extends App {
  println("Start")
  
  val tc = new TextureCreator("/home/pietro909/workspaceJava/AvatarRenderer/out","./","dummy")
 
  val node = tc.makeTree
  tc.composeJson(node, 1024, 768)
  tc.printTexture(node)

  println("End")
}