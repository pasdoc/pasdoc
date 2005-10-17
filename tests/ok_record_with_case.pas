{ @abstract(Testing of parsing and making docs for records with case parts.)

  It does not pass properly yet:
  @orderedList(
    @item(Types for fields in record case are not printed
      (because parser does not set their FullDeclaration properties).)
    @item(Also, CaseTwoB field has no description (but it should have 
      "Description of CaseTwoA and CaseTwoB"))
  )
  
  Update 2005-10-17: now this test passes OK, both problems above
  are solved.
}

unit ok_record_with_case;

interface

type
  TMyRecord1 = record
    { Description of NormalField }
    NormalField: Integer;
    { Description of CaseDecision }
    case CaseDecision: boolean of
      false: (
        { Description of CaseOneSingle }
        CaseOneSingle: Single);
      true: (
        { Description of CaseTwoSingle }
        CaseTwoSingle: Single;
        { Description of CaseTwoInt }
        CaseTwoInt: Integer;
        { Description of CaseTwoA and CaseTwoB }
        CaseTwoA, CaseTwoB: Integer);
  end;

  TMyRecord2 = record
    { Description of NormalField }
    NormalField: Integer;
    case boolean of
      false: (
        { Description of CaseOneSingle }
        CaseOneSingle: Single);
      true: (
        { Description of CaseTwoSingle }
        CaseTwoSingle: Single;
        { Description of CaseTwoInt }
        CaseTwoInt: Integer;
        { Description of CaseTwoA and CaseTwoB }
        CaseTwoA, CaseTwoB: Integer);
  end;

implementation

end.