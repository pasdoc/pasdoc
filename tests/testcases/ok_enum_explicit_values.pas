{ PasDoc fails to parse this. Bug spotted by Michalis on 2005-12-04 when
  trying `make htmldocs' on fpc compiler sources, in file cgbase.pas. 
  
  Fixed. }

unit ok_enum_explicit_values;

interface

type
  TRegister = (
    TRegisterLowEnum := Low(longint),
    TRegisterHighEnum := High(longint)
  );

implementation

end.