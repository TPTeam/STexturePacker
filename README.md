STexturePacker
==============

A texture packer created with Packing Lightmaps algo ( http://www.blackpawn.com/texts/lightmaps/ ).

It generate as output an image SpriteSheet and/or a Json describing the Texture.

Usage example in "Test.scala"



  val tc = new TextureCreator("/home/andrea/ftp/Upload/animation","greit")

  val node = tc.makeTree

  tc.composeJson(node)

  tc.printTexture(node)


