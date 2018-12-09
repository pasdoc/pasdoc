unit ok_not_defined_omit;

interface

{$ifdef NOT_DEFINED}

  { Pasdoc should omit "blah blah blah" below, because "NOT_DEFINED"
    is not defined. But previous bug caused pasdoc into
    missing "$ifdef GLWINDOW_GTK_2", and "$ifdef NOT_DEFINED"
    was closed by earlier "$endif"...
    Fixed now. }

  ({$ifdef GLWINDOW_GTK_2} 0 {$else} #0 {$endif}

  blah blah blah

{$endif read_implementation}

implementation
