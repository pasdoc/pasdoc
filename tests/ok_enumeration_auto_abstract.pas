unit ok_enumeration_auto_abstract;

interface

type
  { Enum type. }
  TEnum = (
    { Enum value 1 }
    en1,

    { Enum value 2. }
    en2,

    { Enum value 3. With longer description }
    en3,

    { Enum value 4. With longer description. }
    en4,

    { Enum value 5.

      With longer description }
    en5,

    { Enum value 6.

      With longer description. }
    en6
  );

implementation

end.