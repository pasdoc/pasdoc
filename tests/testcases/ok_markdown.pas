{ @abstract(**This is a test of Markdown syntax**)
Correct lists

- list item #1.1
- list item #1.2

end list

  - list item #2.1
  - list item #2.2

end list

  * list item #3

end list

  - list item #4

1. list item #5.1
2. list item #5.2
10. list item #5.10

Simple list with multiline text

 - item 6.1
  text
text
     text

 - item 6.2
 
   item text
   
end list

List with nested list

 * item 7.1
   text
   * item 7.1.1
     text
     
   text2

List with more nesting

 - item 8.1
  - item 8.1.1
   - item 8.1.1.1
     
    text 8.1.1
  - item 8.1.2
   - item 8.1.2.1
     
   text 8.1

end list

You can even mix markdown and PasDoc tags

@unorderedList(
- list
 item #8.1
- list
 item #8.2
)

Incorrect lists

*not a list*

1not a list

1 not a list

1.not a list


  Emphasis, aka italics, with *asterisks* or _underscores_.

  Strong emphasis, aka bold, with **asterisks** or __underscores__.

  Some **bold text**.

  Some **_bold and italic text_**.

  Some **some bold and _italic_ and _italic_
    and _italic once again_ text**.

  Some _italic text_.

  Some **bold text with some tags: My name is @name,
    some @code(begin end) and a link to me: @link(ok_markdown)**.

  **Nested formats are **not** supported**

  Some `inline code`.

  Some preformatted code:

  ```
  program Foo;
    Some long code
    with
    syntax highlight
  ```

  Some Pascal code:

  ```pascal
  program Foo;
    Some long code
    with
    syntax highlight
  ```
}
unit ok_markdown;

interface

implementation

end.
