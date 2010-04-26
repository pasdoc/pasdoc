{ @abstract(Test of handling the
  "@italic($I or $INCLUDE : Include compiler info)"
  feature of FPC, see [http://www.freepascal.org/docs-html/prog/progsu38.html].)

  PasDoc bug spotted by Michalis on 2005-12-04 when
  trying `make htmldocs' on fpc compiler sources, in file version.pas.

  Notes about how it should be implemented in PasDoc :

  PasDoc will @italic(not) expand these macros.
  Instead PasDoc will just explicitly show that e.g.
  value of MacDATE is %DATE%, value of MacFPCTARGET is %FPCTARGET% etc.
  Reasons:
  @unorderedList(

    @item(For %DATE% and %TIME%, PasDoc could expand them,
      but it's not sensible. After all, at compilation they will
      be set to something different. So what PasDoc should do
      (and will) is to show user that the value of MacDATE is %DATE%.

      This way user will know that MacDATE's value depends on time
      of compilation.)

    @item(For %FPC???% macros, PasDoc couldn't expand them,
      even if it should. After all, we don't know what FPC version
      will be used to compile the given unit.)

    @item(For %environment-variable%: argument like with
      %FPC???% macros: PasDoc is not able to predict what value
      $environment-variable will have at compilation time.)

    @item(Finally, for %FILE% and %LINE%: this is the only case
      when actually PasDoc could just expand them, just like FPC will.

      For now, my decision is to not expand them,
      for consistency with handling all other %xxx%.)
  )
}

unit ok_include_environment;

interface

const
  MacDATE = {$I %DATE%}; //< Inserts the current date.
  MacFPCTARGET = {$I %FPCTARGET%}; //< Inserts the target CPU name. (deprecated, use FPCTARGETCPU)
  MacFPCTARGETCPU = {$I %FPCTARGETCPU%}; //< Inserts the target CPU name.
  MacFPCTARGETOS = {$I %FPCTARGETOS%}; //< Inserts the target OS name.
  MacFPCVERSION = {$I %FPCVERSION%}; //< Current compiler version number.
  MacFILE = {$I %FILE%}; //< Filename in which the directive is found.
  MacLINE = {$I %LINE%}; //< Linenumer on which the directive is found.
  MacTIME = {$I %TIME%}; //< Current time.

  { If xxx inside %xxx% is none of the above, then it is assumed to be the name
    of an environment variable. Its value will be fetched. }
  MacUSEREnv = {$I %USER%};
  MacPathEnv = {$I %PATH%};

implementation

end.