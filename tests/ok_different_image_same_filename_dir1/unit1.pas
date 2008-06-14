{ @image(image.png)

  This should show description @bold(main) :

  @include(../test_description.txt)

  This should show description @bold(dir_1)  two times:

  @include(test_description.txt)
  @include(./test_description.txt)

  This should show description @bold(dir_2) :

  @include(../ok_different_image_same_filename_dir2/test_description.txt)

}

unit unit1;

interface

implementation

end.