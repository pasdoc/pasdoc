# PasDoc - documentation generator for Pascal source code

## Introduction

_PasDoc_ is a documentation tool for the Object Pascal code.
Documentation is generated from comments found in the source code or in special "description files".
Numerous output formats are supported, including HTML, LaTeX (for PDF, PS), XML and PHP.

This is a _free and open source software_.

## Basic Usage

Put comments before the identifiers in your Pascal source code. Like this:

```pascal
{ My amazing unit. This does @bold(something amazing). }
unit MyUnit;

interface

type
  { My amazing class. }
  TMyClass = class
  private
    FMyProperty: String;
  public
    { My amazing method. Sets @link(MyProperty) to 'foo'. }
    procedure MyMethod;
    { My amazing property. }
    property MyProperty: String read FMyProperty write FMyProperty;
  end;

implementation
// ...
end.
```

Process this source code with PasDoc, and get documentation in one of the output formats. From the command-line, you can do it like this:

```shell
mkdir -p output-dir/
pasdoc myunit.pas --format=html --output=output-dir
```

You can also use the [GUI interface](https://pasdoc.github.io/PasDocGui).

## Support Us

If you find PasDoc useful, we [appreciate your donations](https://castle-engine.io/donate). This helps us to continue improving PasDoc. _Michalis_, maintainer of PasDoc, also makes [Castle Game Engine](https://castle-engine.io/), and you can use the same channels (like [Patreon](https://www.patreon.com/c/castleengine)) to donate to both projects.

## Complete Documentation

See the documentation on [our website](https://pasdoc.github.io/) covering all the features of PasDoc, including the supported comment syntax, output formats, and more.

## License

GNU GPL >= 2.

## Copyright

* copyright (C) 1998-2000 by Marco Schmidt
* copyright (C) 2001-2003 by Ralf Junker <delphi@zeitungsjunge.de>
* Copyright (C) 2003 by Johannes Berg <johannes@sipsolutions.de>
* Copyright 2005-2021 by Michalis Kamburelis, Richard B. Winston and more contributors, see [ChangeLog.md](ChangeLog.md) file.
