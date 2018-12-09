{ This is a test unit that includes ok_relative_include_conf.inc file two times.

  Note that the ok_relative_include_conf.inc file is in the same
  directory as this unit, so @bold(no matter what is current dir
  when running pasdoc) --- you should not need to add any -I
  option to pasdoc to find ok_relative_include_conf.inc. }

unit ok_relative_include_1;

{$I 'ok_relative_include_conf.inc'}

{$I ok_relative_include_conf.inc}

interface

implementation

end.
