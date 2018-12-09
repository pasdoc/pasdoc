{ Obviously this unit is incorrect.

  But the reported error message on 2006-01-12 is also incorrect,
  because line number doesn't count the "//" lines.
  This happens only when this file has UNIX-style line endings
  (#10, not Windows-style #13#10).

  Fixed now.
}

unit error_line_number_2;

//
//
//
//
//
//
//
//
//
//
//
//
//
//
// //
// //
// //
// //
// //
// //
// //
// //
// //
// //
// //
// //

: