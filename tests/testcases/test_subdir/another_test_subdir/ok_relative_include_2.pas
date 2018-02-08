{ This is another test unit that includes
  ok_relative_include_conf.inc file two times.

  This time note that the ok_relative_include_conf.inc file is already
  in the parent directory of this unit file, so once again
  @bold(no matter what is current dir
  when running pasdoc) --- you should not need to add any -I
  option to pasdoc to find ok_relative_include_conf.inc. }

unit ok_relative_include_2;

{$I '../ok_relative_include_conf.inc'}

{$I ../ok_relative_include_conf.inc}

interface

implementation

end.
