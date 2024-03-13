unit ok_if_expressions_2;

{$ifdef FPC}
  {$mode ObjFPC}{$H+}
{$endif}

interface

uses
  Classes, SysUtils;

const _TrueXorTrueIsFalse =
{$IF ((1 = 1) xor (2 = 2)) = 0}
true
{$ELSE}
false
{$ENDIF};

const _3Plus1Is4 =
{$IF 3 + 1 = 4}
true
{$ELSE}
false
{$ENDIF};

const _APlusBIsAB =
{$IF 'a' + 'b' = 'ab'}
true
{$ELSE}
false
{$ENDIF};

const _3Plus2Times2Is7 =
{$IF 3 + 2*2 = 7}
true
{$ELSE}
false
{$ENDIF};

const _3Minus1IsNot4 =
{$IF NOT (3 - 1 = 4)}
true
{$ELSE}
false
{$ENDIF};

const _3Minus2IsTrue =
{$IF 3 - 2}
true
{$ELSE}
false
{$ENDIF};


{ This should be true because NOT 3 = -4 }
const _Not3Plus5IsTrue =
{$IF (NOT 3) + 5}
true
{$ELSE}
false
{$ENDIF};

{ This should be true because NOT as an exception swaps 0 and 1 }
const _Not1Is0 =
{$IF (NOT 1) = 0}
true
{$ELSE}
false
{$ENDIF};

{ This should be true because NOT as an exception swaps 0 and 1 }
const _Not0Is1 =
{$IF (NOT 0) = 1}
true
{$ELSE}
false
{$ENDIF};

const _2Or4Is6 =
{$IF (2 or 4) = 6}
true
{$ELSE}
false
{$ENDIF};

const _6And4Is4 =
{$IF (6 and 4) = 4}
true
{$ELSE}
false
{$ENDIF};

const _2And6Is2 =
{$IF (2 and 6) = 2}
true
{$ELSE}
false
{$ENDIF};

const FPCVersionAtLeast322 =
{$IF FPC_FULLVERSION >= 30202}
true
{$ELSE}
false
{$ENDIF};

const FPCVersionAtLeast422 =
{$IF FPC_FULLVERSION >= 40202}
true
{$ELSE}
false
{$ENDIF};

const
  { Note: This does not compile in FPC.
    But PasDoc, for now, unifies symbols used in $if expressions with macros
    used during normal Pascal code parsing, so it actually replaces
    FPC_FULLVERSION below. }
  MyConst = FPC_FULLVERSION;

implementation

end.
