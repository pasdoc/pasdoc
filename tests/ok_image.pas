{ @abstract(This is @@image tag test.)

  Note that for dvi, eps image will be chosen (for pdf and html, jpg):

  @image(ok_image_picture.eps
         ok_image_picture.jpg)

  Note that using the same image for the 2nd time will cause the same
  image to be included in the output (i.e. in the output we have
  @italic(one) copy of ok_image_picture.jpg, not @italic(two)).

  @image(ok_image_picture.eps
         ok_image_picture.jpg)

  Now note that pdf format will choose pdf version of the image:

  @image(ok_image_picture.eps
         ok_image_picture.pdf
         ok_image_picture.jpg)
}
unit ok_image;

interface

const
  Unimportant = 0;

implementation

end.