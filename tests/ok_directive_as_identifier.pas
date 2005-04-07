{ All calling-convention specifiers must *not* be made links in docs.
  But "Register" procedure name must be made a link.
  Yes, the difficulty is here that "register" is once a
  calling-convention specifier and once a procedure name. 

  This is related to bug submitted to pasdoc-main list
  [http://sourceforge.net/mailarchive/message.php?msg_id=11397611].
}

unit ok_directive_as_identifier;

interface

procedure Register; register;
procedure Cdecl; register;
procedure Foo; register;
procedure Bar; cdecl;

type
  TMyClass = class
  end;

{ Some other test for
  THTMLDocGenerator.WriteCodeWithLinks, while I'm at it:

  Note that link to TMyClass should be correctly made.
  'register' should be displayed as a string, of course, and not linked. }
procedure Foo1(const S: string = 'register'; MyClass: TMyClass);

implementation

end.