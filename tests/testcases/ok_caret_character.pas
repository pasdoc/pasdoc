{ @abstract(Test of parsing "^char" constants,
  see [http://www.freepascal.org/docs-html/ref/refsu7.html].)

  Note that while FPC docs say that only ^A .. ^Z are allowed,
  actually both FPC 2.0.0 and Kylix 3 accept other characters
  after the caret, e.g. "^\" is accepted.

  See also bug report "[ 1358911 ] Invalid character in input stream".
}

unit ok_caret_character;

interface

const
  C2 = ^A;
  C3 = ^B;
  C4 = ^Z;

     // SUBstitute
   SUB = ^Z;
     // ESCape
   ESC = ^[;
     // File Separator
   FS  = ^\;
     // Group Separator
   GS  = ^];
     // Record Separator
   RS  = ^^;
     // Unit Separator
   US  = ^_;

implementation

end.