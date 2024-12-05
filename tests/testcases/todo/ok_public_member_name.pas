{ When advancedrecords are "off",
  "public" is a valid field name in a record for FPC.
  It's actually used by some code:
  packages/wasm-job/examples/job_web.pas has

  ```
  TJSEcdhKeyDeriveParamsRec = record
    public: TJSCryptoKey;
    name: UnicodeString;
  end;
  ```
}

{$ifdef FPC}
  {$mode objfpc}{$H+}{$J-}
  {$modeswitch advancedrecords-}
{$endif}

unit ok_public_member_name;

interface

type
  TJSEcdhKeyDeriveParamsRec = record
    public: Integer;
    name: UnicodeString;
  end;

implementation

end.