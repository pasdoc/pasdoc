unit ok_include_quoted;

{$I 'ok_include_quoted.inc'}

{$I '*.inc'} // should be identical to 'ok_include_quoted.inc'

interface

implementation

end.