{ @abstract(**This is a test of Markdown syntax**)

  Some **bold text**.

  Some **_bold and italic text_**.

  Some **some bold and _italic_ and _italic_
    and _italic once again_ text**.

  Some _italic text_.

  Some **bold text with some tags: My name is @name,
    some @code(begin end) and a link to me: @link(ok_markdown)**.

  **Nested formats are **not** supported**

  Some `inline code`.
  
  ```~
  program Foo;
    Some long code
    with
    syntax highlight
  ~```
}
unit ok_markdown;

interface

implementation

end.