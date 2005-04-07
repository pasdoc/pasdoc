unit warning_tags_no_parameters;

interface

{ Some tags are not allowed to have parameters.

  pasdoc should print a warning when you try to give some parameters
  for such tags: e.g. @nil(parameters that will be ignored), @true(blah). }
procedure Foo;

implementation

end.