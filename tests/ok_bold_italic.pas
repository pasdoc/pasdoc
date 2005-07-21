{ @abstract(@bold(This is a test) of @@bold and @@italic tags.)

  Some @bold(bold text).

  Some @bold(@italic(bold and italic text)).

  Some @bold(some bold and @italic(italic) and @italic(italic)
    and @italic(italic once again) text).

  Some @italic(italic text).

  Some @bold(bold text with some tags: My name is @name,
    some @code(begin end) and a link to me: @link(ok_bold_italic)).

  Some @bold(bold text and @bold(some more)).

  Some @italic(italic text and @italic(some more)).
}
unit ok_bold_italic;

interface

implementation

end.